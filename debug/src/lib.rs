use proc_macro::TokenStream;
use quote::quote;

use syn::{self, parse_macro_input, Data, DataStruct, DeriveInput};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let idents = match input.data {
        Data::Struct(DataStruct { fields, .. }) => fields.into_iter().map(|f| f.ident.unwrap()),
        _ => {
            return syn::Error::new_spanned(input.ident, "we only know structs")
                .into_compile_error()
                .into()
        }
    };

    let fields = idents.map(|f| {
        quote! {
            .field(stringify!(#f), &self.#f)
        }
    });

    let struct_name = input.ident.clone();

    (quote! {
        impl ::std::fmt::Debug for #struct_name {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_name))
                #(#fields)*
                .finish()
            }
        }
    })
    .into()
}
