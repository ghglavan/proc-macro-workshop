use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;

use syn::{
    parse_macro_input, Data, DeriveInput, Field, GenericArgument, Ident, Lit, LitStr, Meta,
    NestedMeta, PathArguments, Type,
};

struct BuilderField {
    name: Ident,
    ty: Type,
    each: Option<(Ident, Type)>,
    optional: Option<Type>,
}

fn get_inner_ty(f: &Field, name: &str) -> Option<Type> {
    match f.ty {
        Type::Path(ref p_ty) if p_ty.qself.is_none() => match p_ty.path.segments.first() {
            Some(p) if p.ident == name => match &p.arguments {
                PathArguments::AngleBracketed(generic) => match generic.args.first() {
                    Some(g) => match g {
                        GenericArgument::Type(t) => Some(t.clone()),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

impl BuilderField {
    pub fn parse(f: &Field) -> Result<Self, TokenStream> {
        let repeated_field: Option<LitStr> = if f.attrs.len() == 0 {
            None
        } else {
            let mut lit = None;

            for attr in &f.attrs {
                let meta = attr.parse_meta();
                match meta {
                    Err(_) => {}
                    Ok(m) => match m {
                        Meta::NameValue(_) | Meta::Path(_) => {}
                        Meta::List(nv) => {
                            for ref meta in nv.nested.iter() {
                                match meta {
                                    NestedMeta::Meta(meta) => match meta {
                                        Meta::NameValue(nv) => match nv.path.get_ident() {
                                            Some(p)
                                                if *p == Ident::new("each", Span::call_site()) =>
                                            {
                                                match &nv.lit {
                                                    Lit::Str(l) => lit = Some(l.clone()),
                                                    _ => {}
                                                }
                                            }
                                            _ => {
                                                return Err(syn::Error::new_spanned(
                                                    attr.parse_meta().unwrap(),
                                                    "expected `builder(each = \"...\")`",
                                                )
                                                .into_compile_error()
                                                .into());
                                            }
                                        },
                                        _ => {}
                                    },
                                    _ => {}
                                }
                            }
                        }
                    },
                }
            }
            lit
        };

        let optional = get_inner_ty(f, "Option");
        let mut each = None;

        if let Some(name) = repeated_field {
            match get_inner_ty(f, "Vec") {
                Some(t) => {
                    let name: Ident = syn::parse_str(&name.value()).unwrap();
                    each = Some((name, t));
                }
                None => {
                    return Err(syn::Error::new_spanned(
                        f,
                        "each should be used only with Vec fields",
                    )
                    .into_compile_error()
                    .into())
                }
            }
        }

        let name = f.ident.clone().unwrap();
        let ty = f.ty.clone();

        Ok(Self {
            name,
            ty,
            each,
            optional,
        })
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive<'a>(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let vis = &input.vis;

    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());
    let data = match input.data {
        Data::Struct(data) => data,
        Data::Enum(_) => {
            return syn::Error::new_spanned(
                &input,
                format!(
                    "cannot derive builder for enum {}. Use a struct",
                    &input.ident
                ),
            )
            .into_compile_error()
            .into()
        }
        Data::Union(_) => {
            return syn::Error::new_spanned(
                &input,
                format!(
                    "cannot derive builder for union {}. Use a struct",
                    &input.ident
                ),
            )
            .into_compile_error()
            .into()
        }
    };

    let mut fields = Vec::new();
    for field in data.fields.iter() {
        match BuilderField::parse(&field) {
            Ok(f) => fields.push(f),
            Err(e) => return e,
        }
    }

    let builder_types = fields.iter().map(|f| {
        let ty = f.ty.clone();
        let name = f.name.clone();

        if f.optional.is_some() || f.each.is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: ::core::option::Option<#ty> }
        }
    });

    let builder_inits = fields.iter().map(|f| {
        let name = f.name.clone();
        quote! { #name: ::core::default::Default::default() }
    });

    let builder_funcs = fields.iter().map(|f| {
        let name = f.name.clone();
        let ty = f.optional.as_ref().unwrap_or(&f.ty).clone();

        if let Some((lit, v_ty)) = &f.each {
            quote! {
                fn #lit(&mut self, #name: #v_ty) -> &mut Self {
                    self.#name.push(#name);
                    self
                }
            }
        } else {
            quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let builder_conds = fields.iter().map(|f| {
        let name = f.name.clone();
        if f.optional.is_some() || f.each.is_some() {
            quote! {}
        } else {
            quote! {
                if self.#name.is_none() {
                    return Err("#name not specified".into());
                }
            }
        }
    });

    let builder_fields_init = fields.iter().map(|f| {
        let name = f.name.clone();

        if f.optional.is_some() {
            quote! {
                #name: self.#name.take()
            }
        } else if f.each.is_some() {
            quote! {
                #name: self.#name.drain(0..).collect()
            }
        } else {
            quote! {
                #name: self.#name.take().unwrap()
            }
        }
    });

    (quote! {
        #vis struct #builder_name {
            #(#builder_types),*
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_inits),*
                }
            }
        }

        impl #builder_name {
            #(#builder_funcs)*
            pub fn build(&mut self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                #(#builder_conds)*
                Ok(
                    #name {
                        #(#builder_fields_init),*
                    }
                  )
            }
        }
    })
    .into()
}
