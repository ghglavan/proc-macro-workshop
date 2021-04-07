use proc_macro::TokenStream;
use proc_macro2::{Span};
use quote::quote;

use syn::{parse_macro_input, Data, DeriveInput, Field, Ident, Type, PathArguments, GenericArgument};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive<'a>(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let vis = &input.vis;
    
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site()); 

    let data = match input.data {
        Data::Struct(data) => data,
        Data::Enum(_) => return syn::Error::new_spanned(&input, format!("cannot derive builder for enum {}. Use a struct", &input.ident)).into_compile_error().into(),
        Data::Union(_) => return syn::Error::new_spanned(&input, format!("cannot derive buolder for union {}. Use a struct", &input.ident)).into_compile_error().into(),
    };

    let fields = data.fields.iter().map(|f| {

        fn get_inner_ty(f: &Field, name: &str) -> Option<Type> {
            match f.ty {
                Type::Path(ref p_ty) if p_ty.qself.is_none() => {
                    match p_ty.path.segments.first() {
                        Some(p) if p.ident == name => {
                            match &p.arguments {
                                PathArguments::AngleBracketed(generic) => {
                                    match generic.args.first() {
                                        Some(g) => {
                                            match g {
                                                GenericArgument::Type(t) => Some(t.clone()),
                                                _ => None
                                            }
                                        },
                                        _ => None
                                    }
                                },
                                _ => None
                            }
                        }
                        _ => None
                    } 
                }, 
                _ => None,
            }
        }
/*
        for attr in &f.attrs {

        }
*/
        let optional =  get_inner_ty(f, "Option");

        (f.ident.clone(), f.ty.clone(), optional)
    }).collect::<Vec<_>>();

    let builder_types = fields.iter().map(|(name, ty, optional)|  {
        let name = name.clone();
        let ty = ty.clone();

        if optional.is_some() {
            quote! { #name: #ty } 
        } else {
            quote! { #name: ::core::option::Option<#ty> }
        }
    });

    let builder_inits = fields.iter().map(|(name, _, _)| {
        let name = name.clone();
        quote! { #name: ::core::default::Default::default() }
    });

    let builder_funcs = fields.iter().map(|(name, ty, optional)| {
        let name = name.clone();
        let ty = optional.as_ref().unwrap_or(&ty).clone();
        
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let builder_conds = fields.iter().map(|(name, _, optional)| {
        let name = name.clone();
        
        if optional.is_some() {
            quote! {}
        } else {
            quote! {
                if self.#name.is_none() {
                    return Err("#name not specified".into());
                }
            }
        }
    });

    let builder_fields_init = fields.iter().map(|(name, _, optional)| {
        let name = name.clone();

        if optional.is_some() {
            quote! {
                #name: self.#name.take()
            }
        } else {
            quote! {
                #name: self.#name.take().unwrap()
            }
        }
        
    });

    (quote!{
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
            pub fn build(&mut self) -> Result<#name, Box<dyn ::std::error::Error>> {
                #(#builder_conds)*
                Ok(
                    #name {
                        #(#builder_fields_init),*
                    }
                  )
            }
        }
    }).into()
}

