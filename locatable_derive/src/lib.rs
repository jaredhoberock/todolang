// locatable_derive/src/lib.rs

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Data, Fields, Index};

#[proc_macro_derive(Locatable)]
pub fn derive_locatable(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let expanded = match input.data {
        Data::Struct(data_struct) => {
            // Handle structs: work for both named and tuple structs.
            let fields = data_struct.fields;
            if fields.is_empty() {
                return syn::Error::new_spanned(name, "Cannot derive Locatable for empty struct")
                    .to_compile_error()
                    .into();
            }
            let (first_access, last_access) = match &fields {
                Fields::Named(fields_named) => {
                    let first_field = fields_named.named.iter().next().unwrap();
                    let last_field = fields_named.named.iter().last().unwrap();
                    let first_ident = first_field.ident.as_ref().unwrap();
                    let last_ident = last_field.ident.as_ref().unwrap();
                    (
                        quote! { &self.#first_ident },
                        quote! { &self.#last_ident },
                    )
                },
                Fields::Unnamed(fields_unnamed) => {
                    let first_index = Index::from(0);
                    let last_index = Index::from(fields_unnamed.unnamed.len() - 1);
                    (
                        quote! { &self.#first_index },
                        quote! { &self.#last_index },
                    )
                },
                Fields::Unit => {
                    return syn::Error::new_spanned(name, "Cannot derive Locatable for unit struct")
                        .to_compile_error()
                        .into();
                },
            };

            quote! {
                impl crate::source_location::Locatable for #name {
                    fn source_span(&self) -> crate::source_location::SourceSpan {
                        let first = #first_access;
                        let last = #last_access;
                        crate::source_location::SourceSpan::new(first.source_span().start, last.source_span().end)
                    }
                }
            }
        },
        Data::Enum(data_enum) => {
            // For each variant, generate a match arm.
            let arms = data_enum.variants.iter().map(|variant| {
                let variant_ident = &variant.ident;
                match &variant.fields {
                    // Newtype variant: Variant(inner)
                    Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                        quote! {
                            #name::#variant_ident(inner) => inner.source_span(),
                        }
                    },
                    // Tuple variant with multiple fields: combine first and last.
                    Fields::Unnamed(fields) => {
                        let first_index = Index::from(0);
                        let last_index = Index::from(fields.unnamed.len() - 1);
                        quote! {
                            #name::#variant_ident(inner) => {
                                let first = &inner.#first_index;
                                let last = &inner.#last_index;
                                crate::source_location::SourceSpan::new(first.source_span().start, last.source_span().end)
                            },
                        }
                    },
                    // Struct variant with one field: Variant { field }
                    Fields::Named(fields) if fields.named.len() == 1 => {
                        let field = fields.named.iter().next().unwrap();
                        let field_ident = field.ident.as_ref().unwrap();
                        quote! {
                            #name::#variant_ident { #field_ident } => #field_ident.source_span(),
                        }
                    },
                    // Struct variant with multiple fields: use first and last fields.
                    Fields::Named(fields) => {
                        let first_field = fields.named.iter().next().unwrap();
                        let last_field = fields.named.iter().last().unwrap();
                        let first_ident = first_field.ident.as_ref().unwrap();
                        let last_ident = last_field.ident.as_ref().unwrap();
                        quote! {
                            #name::#variant_ident { #first_ident, .. } => {
                                crate::source_location::SourceSpan::new(#first_ident.source_span().start, #last_ident.source_span().end)
                            },
                        }
                    },
                    // Unit variant: not supported.
                    Fields::Unit => {
                        syn::Error::new_spanned(variant, "Cannot derive Locatable for unit variant")
                            .to_compile_error()
                    },
                }
            });

            quote! {
                impl crate::source_location::Locatable for #name {
                    fn source_span(&self) -> crate::source_location::SourceSpan {
                        match self {
                            #(#arms)*
                        }
                    }
                }
            }
        },
        Data::Union(_) => {
            return syn::Error::new_spanned(name, "Locatable cannot be derived for unions")
                .to_compile_error()
                .into();
        },
    };

    TokenStream::from(expanded)
}

