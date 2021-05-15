use proc_macro::TokenStream;
use proc_macro2;
use quote::ToTokens;
use syn::{self, Field};

struct Seq {
    ident: syn::Ident,
    begin: syn::LitInt,
    end: syn::LitInt,

    body: proc_macro2::TokenStream,
}

impl syn::parse::Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        input.parse::<syn::Token!(in)>()?;
        let begin = input.parse()?;
        input.parse::<syn::Token!(..)>()?;
        let end = input.parse()?;

        let content;
        syn::braced!(content in input);

        let body = content.parse()?;

        Ok(Self {
            ident,
            begin,
            end,
            body,
        })
    }
}

impl std::fmt::Debug for Seq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Seq")
            .field("ident", &self.ident.to_token_stream().to_string())
            .field("begin", &self.begin.to_token_stream().to_string())
            .field("end", &self.end.to_token_stream().to_string())
            .field("body", &self.body.to_string())
            .finish()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = syn::parse_macro_input!(input as Seq);

    println!("got seq: {:?}", seq);

    (quote::quote! {}).into()
}
