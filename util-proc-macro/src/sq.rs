use proc_macro2::Span;
use quote::quote;
use darling::{FromMeta, ToTokens};
use syn::{AttributeArgs, ItemFn, parse_macro_input, Ident, Visibility, FnArg};

#[derive(Debug, FromMeta)]
pub struct SQFnMacroArgs {
    #[darling(default)]
    vm_var: Option<String>,
    #[darling(default)]
    varargs: Option<String>,
}

pub fn sqfn_impl(
    args: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr_args = parse_macro_input!(args as AttributeArgs);
    let mut item = parse_macro_input!(item as ItemFn);

    let args = match SQFnMacroArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => { return proc_macro::TokenStream::from(e.write_errors()); }
    };

    let original_name = item.sig.ident;
    let original_vis = item.vis;
    item.sig.ident = Ident::new("rust_fn", Span::call_site());
    item.vis = Visibility::from_string("pub").unwrap();

    let norm_argc = item.sig.inputs.len();

    let mut debug_args_fmt = "argc: {}, args: this; ".to_string();

    let normal_args: Vec<_> = item.sig.inputs.iter().cloned().map(
        |a| if let FnArg::Typed(a) = a {
            debug_args_fmt += 
                &format!("{} = {{:?}}; ", &a.pat.to_token_stream().to_string());
            (a.ty, a.pat)
        } else { todo!() }
    ).collect();

    let normal_arg_types = normal_args.iter().map(|(t, _)| t);
    let normal_arg_pats: Vec<_> = normal_args.iter().map(|(_, p)| p).collect();

    let arg_idx = 0..normal_args.len();

    let varargs = match args.varargs {
        Some(s) => {
            let ident = Ident::new(&s, Span::call_site());
            let arg = quote!{ mut #ident: Vec<sq_common::DynSqVar> }.into();
            item.sig.inputs.push(parse_macro_input!( arg as FnArg ));
            debug_args_fmt += &format!("{s} = {{:?}}; ");
            vec![ident]
        } 
        None => vec![],
    };

    let ret_type = match item.sig.output {
        syn::ReturnType::Default => vec![],
        syn::ReturnType::Type(_, ref t) => vec![t.clone()],
    };

    quote! {
        #original_vis struct #original_name;

        impl #original_name {
            #item

            #[allow(unreachable_code, unused_variables, unused)]
            pub unsafe extern "C" fn sq_fn(
                hvm: squirrel2_kaleido_rs::HSQUIRRELVM
            ) -> squirrel2_kaleido_rs::SQInteger {
                use std::collections::HashMap;
                use squirrel2_kaleido_rs::*;
                use sq_common::*;
                use log::debug;

                let mut vm = SQVm::from_handle(hvm);
                // pop unused userdata with method
                vm.pop(1);

                let top = vm.stack_len();
                let norm_argc = #norm_argc as i32;

                #(
                    let idx = #arg_idx as i32 + 2;
                    let #normal_arg_pats: #normal_arg_types = match vm.get(idx) {
                        Ok(a) => a,
                        Err(e) => {
                            vm.throw(e.context(
                                format!("problem with argument {}", idx)
                            ));
                            return -1;
                    }};
                )*

                #( 
                    let mut #varargs = vec![]; 
                    for i in norm_argc+2..=top {
                        let val: DynSqVar = match vm.get(i) {
                            Ok(a) => a,
                            Err(e) => {
                                vm.throw(e.context(
                                    format!("problem with vararg {}", i)
                                ));
                                return -1;
                        }};
                        #varargs.push(val);
                    }
                )*

                debug!(target: stringify!(#original_name),
                    #debug_args_fmt, top, #( #normal_arg_pats ),*, #( #varargs )*
                );

                let ret = Self::rust_fn(#( #normal_arg_pats ),*, #( #varargs )*);

                #( 
                    let _: #ret_type;
                    if let Err(e) = vm.push(ret) {
                        vm.throw(e.context("failed to push return value"));
                        return -1;
                    }
                    return 1;
                )*

                0
            }
        }
    }.into()
}