use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(EnumRef, attributes(ref_name))]
pub fn derive_ref(input: TokenStream) -> TokenStream {
   let input = parse_macro_input!(input as DeriveInput);
   
   // Find the ref_name attribute
   let ref_name = input.attrs.iter()
       .find(|attr| attr.path().is_ident("ref_name"))
       .and_then(|attr| attr.parse_args::<syn::Ident>().ok())
       .unwrap_or_else(|| quote::format_ident!("{}Ref", input.ident));

   let variants = match input.data {
       Data::Enum(ref data) => &data.variants,
       _ => panic!("EnumRef can only be derived for enums"),
   };

   let variant_idents: Vec<_> = variants.iter()
       .map(|v| &v.ident)
       .collect();
   
   let variant_types: Vec<_> = variants.iter()
       .map(|v| {
           match &v.fields {
               Fields::Unnamed(f) => {
                   if f.unnamed.len() != 1 {
                       panic!("Each variant must have exactly one field");
                   }
                   &f.unnamed.first().unwrap().ty
               }
               _ => panic!("Each variant must have exactly one unnamed field"),
           }
       })
       .collect();

   let name = &input.ident;

   let output = quote! {
       use std::ptr::NonNull;

       #[derive(Copy, Clone, Eq, Hash, PartialEq)]
       pub enum #ref_name {
           #(#variant_idents(NonNull<#variant_types>)),*
       }

       // From impls for individual variant types
       #(
           impl From<&#variant_types> for #ref_name {
               fn from(expr: &#variant_types) -> Self {
                   #ref_name::#variant_idents(From::from(expr))
               }
           }
       )*

       // New impl for converting from &Expression
       impl From<&#name> for #ref_name {
           fn from(expr: &#name) -> Self {
               match expr {
                   #(
                       #name::#variant_idents(e) => #ref_name::#variant_idents(From::from(e))
                   ),*
               }
           }
       }
   };

   output.into()
}
