use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, token::Comma, Data, DeriveInput, Error,
    Field, Fields, Visibility,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ = input;

    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let builder = match Builder::try_new(&input) {
        Ok(b) => b,
        Err(e) => return e.into_compile_error().into(),
    };

    let builder_name = &builder.name;
    let builder_struct = builder.struct_def();
    let default_instance = builder.default_instance();
    let setters = builder.setter_methods();
    let build = builder.build_method();

    quote! {
        #builder_struct

        #[automatically_derived]
        impl #builder_name {
            #(#setters)*
            #build
        }

        #[automatically_derived]
        impl #name {
            fn builder() -> #builder_name {
                #default_instance
            }
        }
    }
    .into()
}

struct Builder {
    name: Ident,
    input_name: Ident,
    fields: Punctuated<Field, Comma>,
}

impl Builder {
    fn try_new(input: &DeriveInput) -> Result<Self, Error> {
        let input_name = input.ident.clone();
        let name = Ident::new(&format!("{}Builder", input.ident), input.ident.span());

        let Data::Struct(ds) = &input.data else {
            return Err(Error::new(
                input.ident.span(),
                "derive(Builder) is only supported for structs",
            ));
        };

        let Fields::Named(nf) = ds.fields.clone() else {
            return Err(Error::new(
                input.ident.span(),
                "derive(Builder) does not support tuple or unit structs",
            ));
        };

        let mut fields = nf.named.clone();

        for field in fields.iter_mut() {
            field.vis = Visibility::Inherited;
            field.attrs.clear();
        }

        Ok(Self {
            name,
            input_name,
            fields,
        })
    }

    fn struct_def(&self) -> TokenStream2 {
        let builder_name = &self.name;
        let optionalised_fields = self.fields.iter().map(|field| {
            let mut field = field.clone();
            let curr_ty = field.ty;
            field.ty = parse_quote! { ::std::option::Option<#curr_ty> };
            field
        });

        quote! {
            struct #builder_name {
                #(#optionalised_fields),*
            }
        }
    }

    fn default_instance(&self) -> TokenStream2 {
        let builder_name = &self.name;
        let field_names = self.fields.iter().map(|field| &field.ident);

        quote! {
            #builder_name {
                #(#field_names: ::std::option::Option::None),*
            }
        }
    }

    fn setter_methods(&self) -> Vec<TokenStream2> {
        self.fields
            .iter()
            .map(|field| {
                let name = &field.ident;
                let field_type = &field.ty;
                quote! {
                    fn #name(&mut self, #name: #field_type) -> &mut Self {
                        self.#name = ::std::option::Option::Some(#name);
                        self
                    }
                }
            })
            .collect()
    }

    fn build_method(&self) -> TokenStream2 {
        let name = &self.input_name;
        let extract_values = self.fields.iter().map(|field| {
            let field_name = &field.ident;
            quote! {
                let #field_name = self.#field_name.ok_or("#field_name")?;
            }
        });

        let field_names = self.fields.iter().map(|field| &field.ident);

        quote! {
            fn build(self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                #(#extract_values)*

                std::result::Result::Ok(#name {
                    #(#field_names),*
                })
            }
        }
    }
}
