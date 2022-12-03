use proc_macro2::Span;
use quote::quote;
use darling::{FromMeta, ToTokens};
use syn::{AttributeArgs, ItemFn, parse_macro_input, Ident, Visibility, FnArg, Pat, Path};

#[derive(Debug, FromMeta)]
pub struct SQFnMacroArgs {
    #[darling(default)]
    vm_var: Option<String>,
    #[darling(default)]
    varargs: Option<String>,
    #[darling(default)]
    sq_wrap_path: Option<String>,
    #[darling(default)]
    sq_lib_path: Option<String>,
    #[darling(default)]
    print_args: bool,
    #[darling(default)]
    sqrat_method: bool,
}

#[derive(Debug, FromMeta)]
pub struct SQSetModArgs {
    #[darling(default)]
    sq_wrap_path: Option<String>,
    #[darling(default)]
    sq_lib_path: Option<String>
}

static mut SQ_WRAPPER_MOD: Option<String> = None;
static mut SQ_LIB_MOD: Option<String> = None;

pub fn sq_set_mod_impl(args: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let attr_args = parse_macro_input!(args as AttributeArgs);

    let args = match SQSetModArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => { return proc_macro::TokenStream::from(e.write_errors()); }
    };

    if let Some(s) = args.sq_lib_path {
        unsafe { SQ_LIB_MOD = Some(s) }
    }

    if let Some(s) = args.sq_wrap_path {
        unsafe { SQ_WRAPPER_MOD = Some(s) }
    }

    quote! {}.into()
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

    // meshok vintov
    // intergral shcema
    // programmer book

    let original_name = item.sig.ident;
    let original_vis = item.vis;
    item.sig.ident = Ident::new("rust_fn", Span::call_site());
    item.vis = Visibility::from_string("pub").unwrap();

    let sq_wrapper_mod = {
        let wrapper_path = 
        if let Some(ref s) = args.sq_wrap_path { s }
        else if let Some(s) = unsafe { &SQ_WRAPPER_MOD } { s }
        else { "sq_common" }; 
        
        Path::from_string(wrapper_path).expect("failed to parse sq wrapper path")
    };

    let sq_lib_mod = {
        let lib_path = 
        if let Some(ref s) = args.sq_lib_path { s }
        else if let Some(s) = unsafe { &SQ_LIB_MOD } { s }
        else { "squirrel2_kaleido_rs" }; 

        Path::from_string(lib_path).expect("failed to parse sq lib path")
    };

    let norm_argc = item.sig.inputs.len();

    let mut debug_args_fmt = "argc: {}, args: this; ".to_string();

    let normal_args: Vec<_> = item.sig.inputs.iter().cloned().map(
        |a| if let FnArg::Typed(a) = a {
            debug_args_fmt += 
                &format!("{} = {{:?}}; ", &a.pat.to_token_stream().to_string());
            if let Pat::Ident(p) = *a.pat {
                (a.ty, p.ident)
            } else { todo!("patterns other then Ident is unsupported") }
        } else { todo!() }
    ).collect();

    let normal_arg_types = normal_args.iter().map(|(t, _)| t);
    let normal_arg_idents: Vec<_> = normal_args.iter().map(|(_, p)| p).collect();

    let arg_idx = 0..normal_args.len();

    let varargs = match args.varargs {
        Some(s) => {
            let ident = Ident::new(&s, Span::call_site());
            let arg = quote!{ mut #ident: Vec<#sq_wrapper_mod::DynSqVar> }.into();
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

    let (vm_ident, vm_option) = match args.vm_var {
        Some(s) => {
            let ident = Ident::new(&s, Span::call_site());

            let arg = quote!{ #ident: &mut #sq_wrapper_mod::SQVm }.into();
            item.sig.inputs.push(parse_macro_input!( arg as FnArg ));
            let i = ident.clone();
            (ident, vec![i])
        },
        None => (Ident::new("sqvm", Span::call_site()), vec![]),
    };

    let print_args = args.print_args;
    let sqrat_method = args.sqrat_method;

    quote! {
        #original_vis struct #original_name;

        impl #original_name {
            #item

            #[allow(unreachable_code, unused_variables, unused)]
            pub unsafe extern "C" fn sq_fn(
                hvm: squirrel2_kaleido_rs::HSQUIRRELVM
            ) -> squirrel2_kaleido_rs::SQInteger {
                use log::debug;
                use #sq_lib_mod::*;
                use #sq_wrapper_mod::*;

                let mut #vm_ident = SQVm::from_handle(hvm);

                // Cannot be closed if passed to method
                #vm_ident.set_safety(VmSafety::Friend);

                // pop unused userdata with method
                // TODO: Maybe just write this as last fn arg
                if #sqrat_method {
                    #vm_ident.pop(1); 
                }

                let top = #vm_ident.stack_len();
                let norm_argc = #norm_argc as i32;

                // Stack layout (class method with 2 args): 
                // 1: this TODO: Check if all functions has class or table instance
                // 2: arg0
                // 3: arg1 <-- top
                // ?: popped userdata with method ptr 
                // technically, all functions has varargs by default

                #(  // normal (rust) args indexes: 2..2+norm_argc
                    let idx = #arg_idx as i32 + 2;
                    let #normal_arg_idents: #normal_arg_types = match #vm_ident.get(idx) {
                        Ok(a) => a,
                        Err(e) => {
                            #vm_ident.throw(e.context(
                                format!("problem with argument {}", idx)
                            ));
                            return -1;
                    }};
                )*

                #(  // vararg (rust) indexes: norm_argc+2..=top
                    let mut #varargs = vec![]; 
                    for i in norm_argc+2..=top {
                        let val: DynSqVar = match #vm_ident.get(i) {
                            Ok(a) => a,
                            Err(e) => {
                                #vm_ident.throw(e.context(
                                    format!("problem with vararg {}", i)
                                ));
                                return -1;
                        }};
                        #varargs.push(val);
                    }
                )*

                // Print arguments and their count
                if #print_args {
                    debug!(target: stringify!(#original_name),
                        #debug_args_fmt, top, #( #normal_arg_idents, )* #( #varargs )*
                    );
                }

                let ret = Self::rust_fn(#( #normal_arg_idents, )* #( #varargs,)* #(&mut #vm_option)* );

                // if return type exists, push it and return 1
                #( 
                    let _: #ret_type;
                    if let Err(e) = #vm_ident.push(ret) {
                        #vm_ident.throw(e.context("failed to push return value"));
                        return -1;
                    }
                    return 1;
                )*

                0
            }
        }
    }.into()
}