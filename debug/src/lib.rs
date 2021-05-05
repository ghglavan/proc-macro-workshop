use proc_macro::TokenStream;
use quote::quote;

use proc_macro2;
use syn;

struct DebugField {
    ident: syn::Ident,
    custom_fmt: Option<syn::Lit>,
}

impl DebugField {
    fn parse(field: &syn::Field) -> Result<Self, TokenStream> {
        let ident = field.ident.clone().unwrap();

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

fn add_trait_buond(mut generics: syn::Generics) -> syn::Generics {
    for param in &mut generics.params {
        if let syn::GenericParam::Type(type_param) = param {
            type_param.bounds.push(syn::parse_quote!(::std::fmt::Debug))
        }
    }
    generics
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    let generics = add_trait_buond(input.generics);

    let (impl_gen, type_gen, where_clause) = generics.split_for_impl();

    let fields = match input.data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => fields,
        _ => {
            return syn::Error::new_spanned(input.ident, "we only know structs")
                .into_compile_error()
                .into()
        }
    };

    let idents = fields.iter().map(|f| DebugField::parse(f).unwrap());

    let fields = idents.map(|f| {
        let ident = f.ident.clone();

        match f.custom_fmt {
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
