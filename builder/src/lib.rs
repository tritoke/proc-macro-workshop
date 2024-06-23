use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, Data, DeriveInput, Error, Expr, Field, Fields, GenericArgument, Lit,
    MetaNameValue, PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
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

fn type_extract_inner<'a>(outer_type: &str, r#type: &'a Type) -> Option<&'a Type> {
    let Type::Path(TypePath { path, .. }) = &r#type else {
        return None;
    };

    let PathSegment { ident, arguments } = path.segments.first()?;
    if ident != outer_type {
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

enum BuilderField {
    Normal {
        name: Ident,
        ty: Type,
    },
    Optional {
        name: Ident,
        inner_ty: Type,
    },
    FlattenEach {
        method_name: Ident,
        field_name: Ident,
        inner_ty: Type,
    },
}

impl BuilderField {
    fn parse_each_attribute(field: &Field) -> syn::Result<Option<Ident>> {
        let builder_attr = match field
            .attrs
            .iter()
            .find(|attr| attr.path().is_ident("builder"))
        {
            Some(attr) => attr,
            None => return Ok(None),
        };

        let attr_inner: MetaNameValue = builder_attr.parse_args()?;

        if !attr_inner.path.is_ident("each") {
            return Err(Error::new_spanned(
                builder_attr.meta.clone(),
                "expected `builder(each = \"...\")`",
            ));
        }

        let Expr::Lit(expr_lit) = attr_inner.value else {
            return Err(Error::new_spanned(
                attr_inner,
                "Argument to each must be a literal string",
            ));
        };

        let Lit::Str(lit_str) = expr_lit.lit else {
            return Err(Error::new_spanned(
                expr_lit,
                "Argument to each must be a literal string",
            ));
        };

        lit_str.parse().map(Some)
    }

    fn struct_member(&self) -> TokenStream2 {
        match self {
            BuilderField::Normal { name, ty } | BuilderField::Optional { name, inner_ty: ty } => {
                quote! { #name: ::std::option::Option<#ty>, }
            }
            BuilderField::FlattenEach {
                field_name,
                inner_ty,
                ..
            } => quote! { #field_name: ::std::vec::Vec<#inner_ty>, },
        }
    }

    fn default_assignment(&self) -> TokenStream2 {
        match self {
            BuilderField::Normal { name, .. } | BuilderField::Optional { name, .. } => {
                quote! { #name: ::std::option::Option::None, }
            }
            BuilderField::FlattenEach { field_name, .. } => {
                quote! { #field_name: ::std::vec::Vec::new(), }
            }
        }
    }

    fn setter(&self) -> TokenStream2 {
        match self {
            BuilderField::Normal { name, ty } | BuilderField::Optional { name, inner_ty: ty } => {
                quote! {
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = ::std::option::Option::Some(#name);
                        self
                    }
                }
            }
            BuilderField::FlattenEach {
                method_name,
                field_name,
                inner_ty,
            } => {
                quote! {
                    fn #method_name(&mut self, #method_name: #inner_ty) -> &mut Self {
                        self.#field_name.push(#method_name);
                        self
                    }
                }
            }
        }
    }

    fn extract_field(&self) -> TokenStream2 {
        match self {
            BuilderField::Normal { name, .. } => {
                quote! { let #name = self.#name.take().ok_or(::std::concat!("No value for ", ::std::stringify!(#name)))?; }
            }
            BuilderField::Optional { name, .. } => {
                quote! { let #name = self.#name.take(); }
            }
            BuilderField::FlattenEach { field_name, .. } => {
                quote! {
                let mut #field_name = Vec::new();
                ::std::mem::swap(&mut #field_name, &mut self.#field_name); }
            }
        }
    }

    fn name(&self) -> &Ident {
        match self {
            BuilderField::Normal { name, .. } | BuilderField::Optional { name, .. } => name,
            BuilderField::FlattenEach { field_name, .. } => field_name,
        }
    }
}

impl TryFrom<Field> for BuilderField {
    type Error = Error;

    fn try_from(field: Field) -> syn::Result<Self> {
        let field_name = field.ident.clone().expect("Named field has no name???");

        if let Some(method_name) = Self::parse_each_attribute(&field)? {
            let Some(inner_ty) = type_extract_inner("Vec", &field.ty).cloned() else {
                return Err(Error::new_spanned(
                    field.ty,
                    "Cannot apply each to a member whose type is not Vec<T>",
                ));
            };

            Ok(Self::FlattenEach {
                method_name,
                field_name,
                inner_ty,
            })
        } else if let Some(inner_ty) = type_extract_inner("Option", &field.ty).cloned() {
            Ok(Self::Optional {
                name: field_name,
                inner_ty,
            })
        } else {
            Ok(Self::Normal {
                name: field_name,
                ty: field.ty,
            })
        }
    }
}

struct Builder {
    name: Ident,
    input_name: Ident,
    fields: Vec<BuilderField>,
}

impl Builder {
    fn try_new(input: &DeriveInput) -> syn::Result<Self> {
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

        Ok(Self {
            name,
            input_name,
            fields: nf
                .named
                .into_iter()
                .map(TryFrom::try_from)
                .collect::<syn::Result<_>>()?,
        })
    }

    fn struct_def(&self) -> TokenStream2 {
        let builder_name = &self.name;
        let fields = self.fields.iter().map(BuilderField::struct_member);

        quote! {
            struct #builder_name {
                #(#fields)*
            }
        }
    }

    fn default_instance(&self) -> TokenStream2 {
        let builder_name = &self.name;
        let assignments = self.fields.iter().map(BuilderField::default_assignment);

        quote! {
            #builder_name {
                #(#assignments)*
            }
        }
    }

    fn setter_methods(&self) -> Vec<TokenStream2> {
        self.fields.iter().map(BuilderField::setter).collect()
    }

    fn build_method(&self) -> TokenStream2 {
        let name = &self.input_name;
        let extract_values = self.fields.iter().map(BuilderField::extract_field);
        let field_names = self.fields.iter().map(BuilderField::name);

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
