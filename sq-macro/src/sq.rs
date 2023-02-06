use proc_macro2::Span;
use quote::quote;
use darling::{FromMeta, ToTokens};
use syn::{
    AttributeArgs, ItemFn, parse_macro_input, Ident, Visibility, FnArg, Pat, Path,
    ExprClosure, Type, NestedMeta, parse::{Parse, ParseStream}, Token,
    bracketed, parenthesized, parse_quote
};

const OUTER_CRATE: &str = "sq_common";

#[derive(Debug, FromMeta, Default)]
pub struct SQFnMacroArgs {
    #[darling(default)]
    vm_var: Option<Ident>,
    #[darling(default)]
    varargs: Option<Ident>,
    #[darling(default)]
    print_args: bool,
    #[darling(default)]
    outer_crate: Option<String>,
}

pub enum SqFuncType {
    StaticFn(proc_macro::TokenStream, proc_macro::TokenStream),
    Closure(proc_macro::TokenStream)
}

pub enum ItemSqFn {
    ItemFn(ItemFn),
    Closure(ExprClosure)
}

struct SqClosureInput {
    args_list: Option<Vec<NestedMeta>>,
    closure: ExprClosure,
}

impl SqClosureInput {
    fn parse_args(input: ParseStream) -> syn::Result<Option<AttributeArgs>> {
        let hash_tok = input.parse::<Token![#]>();

        // Assume args not specified
        if hash_tok.is_err() {
            return Ok(None);
        }

        let brack_list;
        let arg_list;
        bracketed!(brack_list in input);
        parenthesized!(arg_list in brack_list);

        // Parse first
        let mut list = vec![arg_list.parse()?];

        while let (Ok(_), Ok(arg)) = (
            arg_list.parse::<Token![,]>(), arg_list.parse::<NestedMeta>()
        ) {
            list.push(arg);
        }

        Ok(Some(list))
    }
}

impl Parse for SqClosureInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let args_list = match Self::parse_args(input) {
            Ok(a) => a,
            Err(e) => return Err(e),
        };
        
        let closure: ExprClosure = input.parse()?;
        Ok(Self {
            args_list,
            closure,
        })
    }
}

pub fn sqfn_impl(
    input: SqFuncType
) -> proc_macro::TokenStream {

    let (args, mut item) = match input {
        SqFuncType::StaticFn(item, args) => {
            let attr_args = parse_macro_input!(args as AttributeArgs);
            
            let args = match SQFnMacroArgs::from_list(&attr_args) {
                Ok(v) => v,
                Err(e) => { return proc_macro::TokenStream::from(e.write_errors()); }
            };

            (args, ItemSqFn::ItemFn(parse_macro_input!(item as ItemFn)))
        },
        SqFuncType::Closure(input) => {
            let parsed = parse_macro_input!(input as SqClosureInput);
            
            let args = if let Some(args_list) = parsed.args_list {
                match SQFnMacroArgs::from_list(&args_list) {
                    Ok(v) => v,
                    Err(e) => { return proc_macro::TokenStream::from(e.write_errors()); }
                }
            } else {
                SQFnMacroArgs::default()
            };

            (args, ItemSqFn::Closure(parsed.closure))
        },
    };

    let pub_vis = Visibility::from_string("pub").unwrap();
    let new_fn_ident = Ident::new("rust_fn", Span::call_site());

    let (original_name, original_vis) = match &mut item {
        ItemSqFn::ItemFn(item) => {
            let mut original_name = new_fn_ident;
            let mut original_vis = pub_vis;
            std::mem::swap(&mut original_name, &mut item.sig.ident);
            std::mem::swap(&mut original_vis, &mut item.vis);
            (original_name, original_vis) 
        },

        // Aren`t needed to create a closure
        ItemSqFn::Closure(_) => (new_fn_ident, pub_vis),
    };

    let norm_argc = match &item {
        ItemSqFn::ItemFn(item) => item.sig.inputs.len(),
        ItemSqFn::Closure(item) => item.inputs.len(),
    };

    let mut debug_args_fmt = "argc: {}, args: this; ".to_string();

    let normal_args: Vec<(Box<Type>, Ident)> = match &item {
        ItemSqFn::ItemFn(item) => item.sig.inputs.iter().cloned().map(
            |a| if let FnArg::Typed(a) = a {
                debug_args_fmt += 
                    &format!("{} = {{:?}}; ", &a.pat.to_token_stream().to_string());
                if let Pat::Ident(p) = *a.pat {
                    (a.ty, p.ident)
                } else { unimplemented!("patterns other then Ident is unsupported") }
            } else { todo!() }
        ).collect(),
        ItemSqFn::Closure(item) => item.inputs.iter().cloned().map(
            |a| if let Pat::Type(a) = a {
                debug_args_fmt += 
                    &format!("{} = {{:?}}; ", &a.pat.to_token_stream().to_string());
                if let Pat::Ident(p) = *a.pat {
                    (a.ty, p.ident)
                } else { unimplemented!("patterns other then Ident is unsupported") } 
            } else { panic!("SQ closure must have typed args") }
        ).collect(),
    };

    let normal_arg_types = normal_args.iter().map(|(t, _)| t);
    let normal_arg_idents: Vec<_> = normal_args.iter().map(|(_, p)| p).collect();

    let arg_idx = 0..normal_args.len();

    let outer_ident = Ident::new(args.outer_crate
        .as_deref()
        .unwrap_or(OUTER_CRATE), Span::call_site()
    );
    let outer: Path = parse_quote! { #outer_ident };
    
    
    let varargs = match args.varargs {
        Some(ident) => {
            let arg = parse_quote! { mut #ident: Vec<#outer::DynSqVar> };
            match &mut item {
                ItemSqFn::ItemFn(item) => item.sig.inputs.push(arg),
                ItemSqFn::Closure(item) => {
                    let FnArg::Typed(a) = arg else { unreachable!() };
                    item.inputs.push(Pat::Type(a));
                }
            };

            debug_args_fmt += &format!("{ident} = {{:?}}; ");
            vec![ident]
        } 
        None => vec![],
    };

    let ret_type = {
        let output = match &item {
            ItemSqFn::ItemFn(item) => &item.sig.output,
            ItemSqFn::Closure(item) => &item.output,
        };

        match output {
            syn::ReturnType::Default => vec![],
            syn::ReturnType::Type(_, ref t) => vec![t.clone()],
        }
    };

    let (vm_ident, vm_option) = match args.vm_var {
        Some(ident) => {
            let arg = parse_quote!{ #ident: &#outer::Vm<#outer::safety::Friend> };
            match &mut item {
                ItemSqFn::ItemFn(item) => item.sig.inputs.push(arg),
                ItemSqFn::Closure(item) => {
                    let FnArg::Typed(a) = arg else { unreachable!() };
                    item.inputs.push(Pat::Type(a));
                }
            };

            let i = ident.clone();
            (ident, vec![i])
        },
        None => (Ident::new("sqvm", Span::call_site()), vec![]),
    };

    let print_args = args.print_args;

    let imports = quote! {
        use log::debug;
        use #outer::*;
        use #outer::error::*;
        use #outer::api::VmRawApi;
    };

    let sq_fn_body_start = quote! {
        let top = #vm_ident.api().stack_top();
        let norm_argc = #norm_argc as isize;

        // Stack layout (function with 2 args):
        // 1: this (bound env)
        // 2: arg0
        // 3: arg1 <-- top
        // technically, all functions has varargs by default

        #(  // normal (rust) args indexes: 2..2+norm_argc
            let idx = #arg_idx as isize + 2;
            let #normal_arg_idents: #normal_arg_types = match #vm_ident.get(idx) {
                Ok(a) => a,
                Err(e) => {
                    let err = SqNativeClosureError::ArgError {
                        idx: #arg_idx as usize,
                        reason: e,
                    };
                    #vm_ident.throw(err);
                    return -1;
            }};
        )*

        #(  // vararg (rust) indexes: norm_argc+2..=top
            let mut #varargs = vec![]; 
            for i in norm_argc+2..=top {
                let val: DynSqVar = match #vm_ident.get(i) {
                    Ok(a) => a,
                    Err(e) => {
                        let err = SqNativeClosureError::VarArgError {
                            idx: i as usize,
                            reason: e
                        };
                        #vm_ident.throw(err);
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
    };

    // Here function called

    let sq_fn_body_end = quote! {
        // if return type exists, push it and return 1
        #( 
            let _: #ret_type;
            if let Err(e) = #vm_ident.push(ret).into_result() {
                #vm_ident.throw(SqNativeClosureError::OutputError(e));
                return -1;
            }
            return 1;
        )*
    
        0
    };

    match item {
        ItemSqFn::ItemFn(item) => quote! {
            #[allow(unreachable_code, unused_mut)]
            #original_vis extern "C" fn #original_name(
                hvm: #outer::api::HSQUIRRELVM
            ) -> isize {
                #imports

                let #vm_ident = unsafe { Vm::from_handle(hvm).into_friend() };

                #item

                #sq_fn_body_start
    
                let ret = rust_fn(#( #normal_arg_idents, )* #( #varargs,)* #(&#vm_option)* );
    
                #sq_fn_body_end
            }
        },
        ItemSqFn::Closure(mut item_clos) => {
            // move `move` keyword to outer closure  
            let move_kw = item_clos.capture;
            item_clos.capture = None;
            quote! {
                Box::new(#move_kw |
                    #vm_ident: &#outer::Vm<#outer::safety::Friend>
                    | -> isize {
                    #imports

                    #sq_fn_body_start

                    // Pop userdata with outer closure ptr
                    #vm_ident.pop(1); 

                    let mut inner = #item_clos;

                    let ret = inner(#( #normal_arg_idents, )* #( #varargs,)* #(#vm_option)* );

                    #sq_fn_body_end
                })
            }
        },
    }.into()
}