use proc_macro::TokenStream;

mod sq;

/// Creates a struct with associated function and it's squirrel wrapper
/// (`::rust_fn` and `::sq_fn`)
#[proc_macro_attribute]
pub fn sqfn(
    args: TokenStream,
    item: TokenStream,
) -> TokenStream {
    sq::sqfn_impl(sq::SqFuncType::StaticFn(item, args))
}

/// Generate boxed squirrel closure  
#[proc_macro] 
pub fn sq_closure(
    item: TokenStream
) -> TokenStream {
    sq::sqfn_impl(sq::SqFuncType::Closure(item))
}