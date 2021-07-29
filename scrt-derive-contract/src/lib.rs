use syn::{
    ItemFn, ItemTrait, AttributeArgs,
    parse_macro_input, parse_quote
};
use quote::quote;

use crate::interface::ContractInterface;

mod interface;
mod utils;

#[proc_macro_attribute]
pub fn contract(args: proc_macro::TokenStream, trait_: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let ast = parse_macro_input!(trait_ as ItemTrait);

    let item_trait = quote!(#ast);

    let interface = ContractInterface::parse(args, ast);
    let boilerplate = interface.generate_boilerplate();

    let result = quote! {
        #item_trait
        #boilerplate
    };

    proc_macro::TokenStream::from(result)
}

#[proc_macro_attribute]
pub fn init(_args: proc_macro::TokenStream, func: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut ast = parse_macro_input!(func as ItemFn);

    add_deps_generics(&mut ast);
    add_fn_args(&mut ast, true);

    let result = quote! {
        #ast
    };

    proc_macro::TokenStream::from(result)
}

#[proc_macro_attribute]
pub fn handle(_args: proc_macro::TokenStream, func: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut ast = parse_macro_input!(func as ItemFn);

    add_deps_generics(&mut ast);
    add_fn_args(&mut ast, true);

    let result = quote! {
        #ast
    };

    proc_macro::TokenStream::from(result)
}

#[proc_macro_attribute]
pub fn query(_args: proc_macro::TokenStream, func: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut ast = parse_macro_input!(func as ItemFn);

    add_deps_generics(&mut ast);
    add_fn_args(&mut ast, false);

    let result = quote! {
        #ast
    };

    proc_macro::TokenStream::from(result)
}

fn add_deps_generics(func: &mut ItemFn) {    
    func.sig.generics.params.push(parse_quote!(S: cosmwasm_std::Storage));
    func.sig.generics.params.push(parse_quote!(A: cosmwasm_std::Api));
    func.sig.generics.params.push(parse_quote!(Q: cosmwasm_std::Querier));
}

fn add_fn_args(func: &mut ItemFn, is_tx: bool) {
    if is_tx {
        func.sig.inputs.push(parse_quote!(deps: &mut cosmwasm_std::Extern<S, A, Q>));
        func.sig.inputs.push(parse_quote!(env: cosmwasm_std::Env));
    } else {
        func.sig.inputs.push(parse_quote!(deps: &cosmwasm_std::Extern<S, A, Q>));
    }
}
