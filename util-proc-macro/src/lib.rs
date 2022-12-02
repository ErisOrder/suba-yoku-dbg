use proc_macro::TokenStream;
use sq::sq_set_mod_impl;

mod sq;

/// Creates a struct with associated function and it's squirrel wrapper
/// (`::rust_fn` and `::sq_fn`)
#[proc_macro_attribute]
pub fn sqfn(
  args: TokenStream,
  item: TokenStream,
) -> TokenStream {
  sq::sqfn_impl(args, item)
}

/// Set sq_lib_path and sq_wrap_path
#[proc_macro] 
pub fn set_sqfn_paths(
  args: TokenStream
) -> TokenStream {
  sq_set_mod_impl(args)
}