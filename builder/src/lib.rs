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

    quote! {
        #builder_struct

        #[automatically_derived]
        impl #builder_name {
            #(#setters)*
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
    fields: Punctuated<Field, Comma>,
}

impl Builder {
    fn try_new(input: &DeriveInput) -> Result<Self, Error> {
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

        Ok(Self { name, fields })
    }

    fn struct_def(&self) -> TokenStream2 {
        let builder_name = &self.name;
        let optionalised_fields = self.fields.iter().map(|field| {
            let mut field = field.clone();
            let curr_ty = field.ty;
            field.ty = parse_quote! { Option<#curr_ty> };
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
                #(#field_names: None),*
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
                        self.#name = Some(#name);
                        self
                    }
                }
            })
            .collect()
    }
}
