use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn};

#[proc_macro_attribute]
pub fn restore_self_on_err(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    let name = &input.sig.ident;
    let visibility = &input.vis;
    let inputs = &input.sig.inputs;
    let output = &input.sig.output;
    let body = &input.block;

    let expanded = quote! {
        #visibility fn #name(#inputs) #output {
            let original_state = self.clone();
            let result = (|| #body)();
            if result.is_err() {
                *self = original_state;
            }
            result
        }
    };

    TokenStream::from(expanded)
}

