use syn::{
    TraitItemMethod, Path, NestedMeta, AttributeArgs, ItemTrait,
    Meta, TraitItem, AttrStyle, ReturnType, Type, Ident, ItemEnum,
    Variant, FnArg, FieldsNamed, Field, Visibility, Pat, Fields,
    ItemStruct, ItemFn, Stmt, Expr, ExprMatch, ExprField,
    GenericArgument, PathArguments, parse_quote
};
use syn::token::{Comma, Brace, Colon};
use syn::punctuated::Punctuated;
use quote::quote;
use proc_macro2::{TokenStream, Span};

use crate::utils::to_pascal;

macro_rules! default_impl_ident {
    ($msg:expr) => {
        Ident::new(&format!("Default{}Impl", $msg), Span::call_site())
    };
}

const INIT_MSG: &str = "InitMsg";
const HANDLE_MSG: &str = "HandleMsg";
const QUERY_MSG: &str = "QueryMsg";
const CONTRACT_ARG: &str = "contract";

pub struct ContractInterface {
    ident: Ident,
    init: TraitItemMethod,
    handle: Vec<TraitItemMethod>,
    query: Vec<TraitItemMethod>
}

#[derive(Clone)]
enum MsgType {
    Handle,
    Query
}

impl MsgType {
    pub fn to_ident(&self) -> Ident {
        match self {
            Self::Handle => Ident::new(HANDLE_MSG, Span::call_site()),
            Self::Query => Ident::new(QUERY_MSG, Span::call_site())
        }
    }
}

impl ContractInterface {
    pub fn parse(args: AttributeArgs, item_trait: ItemTrait) -> Self {
        let response = get_response_type(args);
        
        let mut init: Option<TraitItemMethod> = None;
        let mut handle: Vec<TraitItemMethod> = vec![];
        let mut query: Vec<TraitItemMethod> = vec![];
    
        for item in item_trait.items.into_iter() {
            if let TraitItem::Method(method) = item {
                for attr in method.attrs.iter() {
                    if let AttrStyle::Inner(_) = attr.style {
                        panic!("Invalid attribute style")
                    }

                    let ref path = attr.path.segments.last().unwrap();
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
    
        Self {
            ident: item_trait.ident,
            init: init.expect("Expecting one method to be annotated as #[init]"),
            handle,
            query
        }    
    }

    pub fn generate_boilerplate(&self) -> TokenStream {
        let struct_impl = self.generate_default_impl();

        let init_msg = self.generate_init_msg();
        let handle_msg = self.generate_messages(MsgType::Handle);
        let query_msg = self.generate_messages(MsgType::Query);

        let init = self.generate_init_fn();
        let handle = self.generate_handle_fn();
        let query = self.generate_query_fn();

        quote! {
            #struct_impl
            #init_msg
            #handle_msg
            #query_msg
            #init
            #handle
            #query
        }
    }

    fn generate_default_impl(&self) -> TokenStream {
        let struct_ident = default_impl_ident!(self.ident.to_string());
        let ref trait_ident = self.ident;

        quote! {
            pub struct #struct_ident;

            impl #trait_ident for #struct_ident { }
        }
    }

    fn generate_messages(&self, msg_type: MsgType) -> ItemEnum {
        let methods = match msg_type {
            MsgType::Handle => &self.handle,
            MsgType::Query => &self.query
        };

        let enum_name = msg_type.to_ident();

        let mut result: ItemEnum = parse_quote!{
            #[derive(serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
            #[serde(rename_all = "snake_case")]
            pub enum #enum_name {

            }
        };

        for method in methods {
            let variant_name = to_pascal(&method.sig.ident.to_string());
            let fields = extract_fields(method);

            result.variants.push(Variant {
                attrs: vec![],
                ident: Ident::new(&variant_name, Span::call_site()),
                fields,
                discriminant: None
            });
        }
        
        result
    }

    fn generate_init_msg(&self) -> ItemStruct {
        let msg = Ident::new(INIT_MSG, Span::call_site());

        let mut result: ItemStruct = parse_quote!{
            #[derive(serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
            pub struct #msg {

            }
        };

        result.fields = extract_fields(&self.init);

        result
    }
    
    fn generate_init_fn(&self) -> ItemFn {
        let msg = Ident::new(INIT_MSG, Span::call_site());
        let arg = self.create_trait_arg();

        let mut result: ItemFn = parse_quote! {
            pub fn init<S: cosmwasm_std::Storage, A: cosmwasm_std::Api, Q: cosmwasm_std::Querier>(
                deps: &mut cosmwasm_std::Extern<S, A, Q>,
                env: cosmwasm_std::Env,
                msg: #msg,
                #arg
            ) -> StdResult<InitResponse> { }
        };

        let ref method_name = self.init.sig.ident;

        let mut args = Punctuated::<ExprField, Comma>::new();

        for input in &self.init.sig.inputs {
            let ident = extract_fn_arg_ident(input);
            args.push_value(parse_quote!(msg.#ident));
            args.push_punct(Comma(Span::call_site()));
        }

        let arg_name = Ident::new(CONTRACT_ARG, Span::call_site());

        let call: Expr = parse_quote!(#arg_name.#method_name(#args deps, env));
        result.block.stmts.push(Stmt::Expr(call));

        result
    }
    
    fn generate_handle_fn(&self) -> ItemFn {
        let msg = MsgType::Handle.to_ident();
        let arg = self.create_trait_arg();

        let mut result: ItemFn = parse_quote! {
            pub fn handle<S: cosmwasm_std::Storage, A: cosmwasm_std::Api, Q: cosmwasm_std::Querier>(
                deps: &mut cosmwasm_std::Extern<S, A, Q>,
                env: cosmwasm_std::Env,
                msg: #msg,
                #arg
            ) -> StdResult<cosmwasm_std::HandleResponse> { }
        };

        let match_expr = self.create_match_expr(MsgType::Handle);
        result.block.stmts.push(Stmt::Expr(match_expr));
        
        result
    }

    fn generate_query_fn(&self) -> ItemFn {
        let msg = MsgType::Query.to_ident();
        let arg = self.create_trait_arg();

        let mut result: ItemFn = parse_quote! {
            pub fn query<S: cosmwasm_std::Storage, A: cosmwasm_std::Api, Q: cosmwasm_std::Querier>(
                deps: &cosmwasm_std::Extern<S, A, Q>,
                msg: #msg,
                #arg
            ) -> cosmwasm_std::QueryResult { }
        };

        let match_expr = self.create_match_expr(MsgType::Query);
        result.block.stmts.push(Stmt::Expr(match_expr));

        result
    }

    fn create_match_expr(&self, msg_type: MsgType) -> Expr {
        let methods = match msg_type {
            MsgType::Handle => &self.handle,
            MsgType::Query => &self.query
        };

        let enum_name = msg_type.to_ident();
        let mut match_expr: ExprMatch = parse_quote!(match msg {});

        for method in methods {
            let ref method_name = method.sig.ident;

            let variant = to_pascal(&method_name.to_string());
            let variant = Ident::new(&variant, Span::call_site());

            let mut args = Punctuated::<Ident, Comma>::new();

            for input in &method.sig.inputs {
                let ident = extract_fn_arg_ident(input);
                args.push_value(ident);
                args.push_punct(Comma(Span::call_site()));
            }

            let arg_name = Ident::new(CONTRACT_ARG, Span::call_site());

            match msg_type {
                MsgType::Handle => {
                    match_expr.arms.push(
                        parse_quote!(#enum_name::#variant { #args } => #arg_name.#method_name(#args deps, env))
                    );
                },
                MsgType::Query => {
                    match_expr.arms.push(
                        parse_quote!(#enum_name::#variant { #args } => cosmwasm_std::to_binary(&#arg_name.#method_name(#args deps)?))
                    );
                }
            }
        }

        Expr::Match(match_expr)
    }
    
    fn create_trait_arg(&self) -> FnArg {
        let ref trait_name = self.ident;
        let arg_name = Ident::new(CONTRACT_ARG, Span::call_site());

        parse_quote!(#arg_name: impl #trait_name)
    }
}

fn extract_fields(method: &TraitItemMethod) -> Fields {
    let mut fields = FieldsNamed {
        brace_token: Brace(Span::call_site()),
        named: Punctuated::<Field, Comma>::default()
    };

    for arg in method.sig.inputs.iter() {
        match arg {
            FnArg::Typed(pat_type) => {
                let ident = match *pat_type.pat.to_owned() {
                    Pat::Ident(pat_ident) => pat_ident.ident,
                    _ => panic!("Expected identifier.")
                };

                fields.named.push(Field {
                    attrs: vec![],
                    vis: Visibility::Inherited,
                    ident: Some(ident),
                    ty: *pat_type.ty.to_owned(),
                    colon_token: Some(Colon(Span::call_site()))
                });
            },
            FnArg::Receiver(_) => panic!("Method definition cannot contain \"self\"")
        }
    }

    Fields::Named(fields)
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

fn validate_return_type(method: &TraitItemMethod, path: &Path) {
    if let ReturnType::Type(_, return_type) = &method.sig.output {
        if let Type::Path(return_type_path) = return_type.as_ref() {
            assert!(return_type_path.qself.is_none(), "Unexpected \"Self\" in return type.");

            let last = return_type_path.path.segments.last().unwrap();
            
            if last.ident.to_string().as_str() == "StdResult" {
                if let PathArguments::AngleBracketed(args) = &last.arguments {
                    if let GenericArgument::Type(ty) =  &args.args[0] {
                        if let Type::Path(generic_path) = ty {
                            let ref generic_ident = generic_path.path.segments.last().unwrap().ident;
                            let ref expected = path.segments.last().unwrap().ident;
                            
                            if *generic_ident == *expected {
                                return;
                            }
                        }
                    }
                }
            }
        }
    }

    let expected_type = format!("{}", quote!{ #path });
    panic!("Expecting return type: StdResult<{}>", expected_type);
}

fn extract_fn_arg_ident(arg: &FnArg) -> Ident {
    match arg {
        FnArg::Typed(pat_type) => {
            match *pat_type.pat.to_owned() {
                Pat::Ident(pat_ident) => return pat_ident.ident,
                _ => panic!("Expected identifier.")
            };
        },
        FnArg::Receiver(_) => panic!("Method definition cannot contain \"self\"")
    }
}
