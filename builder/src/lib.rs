use proc_macro::TokenStream;
use syn::DeriveInput;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as DeriveInput);
    eprintln!("{st:#?}");
    TokenStream::new()
}
