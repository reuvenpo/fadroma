use syn::{
    ItemFn, ItemTrait, AttributeArgs, NestedMeta, Meta,
    Path, TraitItem, TraitItemMethod, AttrStyle, ReturnType,
    Type, parse_macro_input, parse_quote
};
use quote::quote;

#[derive(Debug)]
struct ContractInterface {
    init: TraitItemMethod,
    handle: Vec<TraitItemMethod>,
    query: Vec<TraitItemMethod>,
    response: Path
}

#[proc_macro_attribute]
pub fn contract(args: proc_macro::TokenStream, trait_: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let ast = parse_macro_input!(trait_ as ItemTrait);

    let interface = get_contract_interface(args, ast);
    println!("{:?}", interface);

    let result = quote! {
        enum Todo {
            One,
            Two
        }
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

fn get_response_type(args: AttributeArgs) -> Path {
    if args.len() != 1 {
        panic!("Expected exactly 1 argument in \"contract\" attribute.")
    }

    if let NestedMeta::Meta(meta) = args.first().unwrap().to_owned() {
        if let Meta::Path(path) = meta {
            return path;
        }
    }

    panic!("Expected a type argument in \"contract\" attribute.")
}

fn get_contract_interface(args: AttributeArgs, ast: ItemTrait) -> ContractInterface {
    let response = get_response_type(args);
    let mut init: Option<TraitItemMethod> = None;
    let mut handle: Vec<TraitItemMethod> = vec![];
    let mut query: Vec<TraitItemMethod> = vec![];

    for item in ast.items.into_iter() {
        if let TraitItem::Method(method) = item {
            for attr in method.attrs.iter() {
                if let AttrStyle::Inner(_) = attr.style {
                    panic!("Invalid attribute style")
                }

                // TODO: need more robust detection that includes namespaces as well
                // also, there is probably a better way to do this
                let ref path = attr.path;
                let path = format!("{}", quote!{ #path });

                match path.as_str() {
                    "init" => {
                        if init.is_some() {
                            panic!("Only one method can be annotated as #[init].")
                        }

                        validate_return_type(&method, &parse_quote!(InitResponse));
                        init = Some(method);

                        break;
                    },
                    "handle" => {
                        validate_return_type(&method, &parse_quote!(HandleResponse));
                        handle.push(method);

                        break;
                    },
                    "query" => {
                        validate_return_type(&method, &response);
                        query.push(method);

                        break;
                    },
                    _ => continue
                }
            }
        }
    }

    ContractInterface {
        init: init.expect("Expecting one method to be annotated as #[init]"),
        handle,
        query,
        response
    }    
}

fn validate_return_type(method: &TraitItemMethod, path: &Path) {
    if let ReturnType::Type(_, return_type) = &method.sig.output {
        if let Type::Path(return_type_path) = return_type.as_ref() {
            // TODO: namespaces?
            let path: Path = parse_quote!(StdResult<#path>);

            if path == return_type_path.path {
                return;
            }
        }
    }

    let expected_type = format!("{}", quote!{ #path });
    panic!("Expecting return type: StdResult<{}>", expected_type);
}
