use proc_macro::TokenStream;

mod sq;

#[proc_macro_attribute]
pub fn sqfn(
  args: TokenStream,
  item: TokenStream,
) -> TokenStream {
  sq::sqfn_impl(args, item)
}