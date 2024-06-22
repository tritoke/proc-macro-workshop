use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, token::Comma, Data, DeriveInput, Error,
    Field, Fields, GenericArgument, PathArguments, PathSegment, Type, TypePath, Visibility,
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

fn type_extract_option_inner(r#type: &Type) -> Option<&Type> {
    let Type::Path(TypePath { path, .. }) = &r#type else {
        return None;
    };

    let PathSegment { ident, arguments } = path.segments.first()?;
    if ident != "Option" {
        return None;
    }

    let PathArguments::AngleBracketed(args_inner) = arguments else {
        return None;
    };

    let GenericArgument::Type(inner_type) = args_inner.args.first()? else {
        return None;
    };

    Some(inner_type)
}

impl Builder {
    fn try_new(input: &DeriveInput) -> Result<Self, Error> {
        let input_name = input.ident.clone();
        let name = Ident::new(&format!("{}Builder", input.ident), Span::call_site());

        let Data::Struct(ds) = &input.data else {
            return Err(Error::new(
                Span::call_site(),
                "derive(Builder) is only supported for structs",
            ));
        };

        let Fields::Named(nf) = ds.fields.clone() else {
            return Err(Error::new(
                Span::call_site(),
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
            let curr_ty_or_inner = type_extract_option_inner(&field.ty).unwrap_or(&field.ty);
            field.ty = parse_quote! { ::std::option::Option<#curr_ty_or_inner> };
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
                let arg_type = type_extract_option_inner(&field.ty).unwrap_or(&field.ty);
                quote! {
                    fn #name(&mut self, #name: #arg_type) -> &mut Self {
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
            let field_value = if type_extract_option_inner(&field.ty).is_some() {
                quote! {
                    self.#field_name.take()
                }
            } else {
                quote! {
                    self.#field_name.take().ok_or("No value for #field_name")?
                }
            };
            quote! {
                let #field_name = #field_value;
            }
        });

        let field_names = self.fields.iter().map(|field| &field.ident);

        quote! {
            fn build(&mut self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                #(#extract_values)*

                std::result::Result::Ok(#name {
                    #(#field_names),*
                })
            }
        }
    }
}
