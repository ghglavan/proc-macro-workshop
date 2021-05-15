use proc_macro::TokenStream;
use quote::{quote, ToTokens};

use proc_macro2;
use syn;

struct DebugField {
    ident: syn::Ident,
    custom_fmt: Option<syn::Lit>,
}

fn is_type_in_path(t: &syn::Ident, p: &syn::Path) -> bool {
    if p.is_ident(t) {
        return true;
    }

    for segment in p.segments.iter() {
        if let syn::PathArguments::AngleBracketed(angle_args) = &segment.arguments {
            for args in angle_args.args.iter() {
                match args {
                    syn::GenericArgument::Type(syn::Type::Path(p)) => {
                        return is_type_in_path(t, &p.path);
                    }
                    _ => {}
                }
            }
        }
    }

    return false;
}

fn get_associated_types_in_path(t: &syn::Ident, p: &syn::Path) -> Vec<syn::Path> {
    let mut v = Vec::new();

    for segment in p.segments.iter() {
        match &segment.arguments {
            syn::PathArguments::AngleBracketed(angle_args) => {
                for args in angle_args.args.iter() {
                    match args {
                        syn::GenericArgument::Type(syn::Type::Path(p)) => {
                            v.append(&mut get_associated_types_in_path(t, &p.path));
                        }
                        _ => {}
                    }
                }
            }
            syn::PathArguments::None if segment.ident == *t => v.push(p.clone()),
            _ => {}
        }
    }

    v
}

fn get_attr(attrs: &Vec<syn::Attribute>, name: &str) -> Option<syn::LitStr> {
    let mut lit = None;

    for attr in attrs.iter() {
        let meta = attr.parse_meta();
        match meta {
            Err(_) => {}
            Ok(m) => match m {
                syn::Meta::NameValue(_) | syn::Meta::Path(_) => {}
                syn::Meta::List(nv) => {
                    for ref meta in nv.nested.iter() {
                        match meta {
                            syn::NestedMeta::Meta(meta) => match meta {
                                syn::Meta::NameValue(nv) => match nv.path.get_ident() {
                                    Some(p)
                                        if *p
                                            == syn::Ident::new(
                                                name,
                                                proc_macro2::Span::call_site(),
                                            ) =>
                                    {
                                        match &nv.lit {
                                            syn::Lit::Str(l) => lit = Some(l.clone()),
                                            _ => {}
                                        }
                                    }
                                    _ => {
                                        return None;
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
}

fn is_segment_in_path(segment: &syn::PathSegment, path: &syn::Path) -> bool {
    for seg in path.segments.iter() {
        if seg.ident == segment.ident {
            return true;
        }
    }

    return false;
}

fn add_where_clause(params: &mut syn::Generics, path: &syn::Path, bound: &syn::TypeParamBound) {
    let mut bounds = syn::punctuated::Punctuated::new();
    bounds.push(bound.clone());
    let where_predicate = syn::WherePredicate::Type(syn::PredicateType {
        lifetimes: None,
        bounded_ty: syn::Type::Path(syn::TypePath {
            qself: None,
            path: path.clone(),
        }),
        colon_token: syn::token::Colon {
            spans: [proc_macro2::Span::call_site()],
        },
        bounds,
    });

    match params.where_clause {
        Some(ref mut clause) => {
            let mut should_push = true;
            for pred in clause.predicates.iter() {
                match pred {
                    syn::WherePredicate::Type(syn::PredicateType {
                        bounded_ty: syn::Type::Path(syn::TypePath { path, .. }),
                        ..
                    }) => {
                        for segment in path.segments.iter() {
                            if is_segment_in_path(segment, path) {
                                should_push = false;
                            }
                        }
                    }
                    _ => {}
                }
            }
            if should_push {
                clause.predicates.push(where_predicate);
            }
        }
        None => {
            let mut predicates = syn::punctuated::Punctuated::new();
            predicates.push(where_predicate);
            params.where_clause = Some(syn::WhereClause {
                where_token: syn::token::Where {
                    span: proc_macro2::Span::call_site(),
                },
                predicates,
            })
        }
    }
}

impl DebugField {
    fn parse(field: &syn::Field, params: &mut syn::Generics) -> Result<Self, TokenStream> {
        let ident = field.ident.clone().unwrap();

        let mut new_assoc_types = Vec::new();
        match &field.ty {
            syn::Type::Path(p) => {
                for param in params.params.iter_mut() {
                    if let syn::GenericParam::Type(t) = param {
                        match p.path.segments.iter().last() {
                            Some(s)
                                if s.ident
                                    == syn::Ident::new(
                                        "PhantomData",
                                        proc_macro2::Span::call_site(),
                                    ) => {}
                            _ => {
                                if is_type_in_path(&t.ident, &p.path) {
                                    t.bounds.push(syn::parse_quote!(::std::fmt::Debug))
                                }

                                new_assoc_types
                                    .append(&mut get_associated_types_in_path(&t.ident, &p.path));
                            }
                        }
                    }
                }
            }
            _ => {}
        };

        for ty_path in new_assoc_types {
            add_where_clause(params, &ty_path, &syn::parse_quote!(::std::fmt::Debug));
        }

        let mut custom_fmt = None;

        for attr in field.attrs.iter() {
            match attr.parse_meta() {
                Ok(syn::Meta::NameValue(nv)) => {
                    if nv
                        .path
                        .is_ident(&syn::Ident::new("debug", proc_macro2::Span::call_site()))
                    {
                        custom_fmt = Some(nv.lit.clone());
                    }
                }
                Ok(_) => {
                    return Err(syn::Error::new_spanned(attr, "expected attr as ident")
                        .into_compile_error()
                        .into());
                }
                Err(e) => {
                    return Err(e.into_compile_error().into());
                }
            }
        }

        Ok(Self { ident, custom_fmt })
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let mut input = syn::parse_macro_input!(input as syn::DeriveInput);

    if let Some(lit) = get_attr(&input.attrs, "bound") {
        let val = lit.value();
        let ts = val.split(": ").collect::<Vec<&str>>();

        let path: syn::Path = syn::parse_str(ts[0]).unwrap();
        let bound: syn::TypeParamBound = syn::parse_str(ts[1]).unwrap();

        add_where_clause(&mut input.generics, &path, &bound);
    }

    let fields = match input.data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => fields,
        _ => {
            return syn::Error::new_spanned(input.ident, "we only know structs")
                .into_compile_error()
                .into()
        }
    };

    let mut idents = Vec::new();

    for field in fields.iter() {
        idents.push(DebugField::parse(field, &mut input.generics).unwrap());
    }

    let (impl_gen, type_gen, where_clause) = input.generics.split_for_impl();

    let fields = idents.iter().map(|f| {
        let ident = f.ident.clone();

        match &f.custom_fmt {
            Some(l) => {
                quote! {
                    .field(stringify!(#ident), &::std::format_args!(#l , &self.#ident))
                }
            }
            None => {
                quote! {
                    .field(stringify!(#ident), &self.#ident)
                }
            }
        }
    });

    let struct_name = input.ident.clone();

    (quote! {
        impl #impl_gen ::std::fmt::Debug for #struct_name #type_gen #where_clause {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_name))
                #(#fields)*
                .finish()
            }
        }
    })
    .into()
}
