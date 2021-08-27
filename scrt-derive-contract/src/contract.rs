use syn::{
    TraitItemMethod, Path, AttributeArgs, ItemTrait, Meta, Lit,
    TraitItem, AttrStyle, ReturnType, Type, Ident, ItemEnum, TypePath,
    Variant, FnArg, FieldsNamed, Field, Visibility, Pat, NestedMeta,
    Fields, ItemStruct, ItemFn, Stmt, Expr, ExprMatch, ExprField,
    GenericArgument, PathArguments, parse_quote
};
use syn::token::{Comma, Brace, Colon};
use syn::punctuated::Punctuated;
use quote::quote;
use proc_macro2::{TokenStream, Span};

use crate::args::ContractArgs;
use crate::utils::to_pascal;

pub const INIT_MSG: &str = "InitMsg";
pub const HANDLE_MSG: &str = "HandleMsg";
pub const QUERY_MSG: &str = "QueryMsg";
pub const RESPONSE_MSG: &str = "QueryResponse";
pub const DEFAULT_IMPL_STRUCT: &str = "DefaultImpl";
pub const INIT_FN: &str = "init";
pub const HANDLE_FN: &str = "handle";
pub const QUERY_FN: &str = "query";

const CONTRACT_ARG: &str = "contract";

pub struct Contract {
    ty: ContractType,
    args: ContractArgs,
    ident: Ident,
    /// Optional because a component might not want to have an init method.
    init: Option<TraitItemMethod>,
    handle: Vec<TraitItemMethod>,
    query: Vec<TraitItemMethod>
}

#[derive(Clone, Copy)]
pub enum ContractType {
    /// A contract that directly implements its functionality.
    Contract,
    /// An interface defines the methods that a contract exposes.
    Interface,
    /// A contract that implements an interface.
    Impl
}

#[derive(Clone, Copy)]
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

impl Contract {
    pub fn parse(args: AttributeArgs, item_trait: ItemTrait, ty: ContractType) -> Self {
        let args = ContractArgs::parse(args);
        
        let mut init = None;
        let mut handle = vec![];
        let mut query = vec![];
    
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
                            assert!(init.is_none(), "Only one method can be annotated as #[init].");
    
                            validate_method(&method, Some(parse_quote!(InitResponse)), ty);
                            init = Some(method);
    
                            break;
                        },
                        "handle" => {
                            validate_method(&method, Some(parse_quote!(HandleResponse)), ty);
                            handle.push(method);
    
                            break;
                        },
                        "query" => {
                            validate_method(&method, None, ty);
                            query.push(method);
    
                            break;
                        },
                        _ => continue
                    }
                }
            }
        }
    
        Self {
            ty,
            args,
            ident: item_trait.ident,
            init,
            handle,
            query
        }    
    }

    pub fn generate_boilerplate(&self) -> TokenStream {
        let init_msg = self.generate_init_msg();
        let handle_msg = self.generate_messages(MsgType::Handle);
        let query_msg = self.generate_messages(MsgType::Query);
        let query_response = self.generate_response_msg();

        if let ContractType::Interface = self.ty {
            return quote! {
                #init_msg
                #handle_msg
                #query_msg
                #query_response
            };
        }

        let struct_impl = self.generate_default_impl();

        let init = self.generate_init_fn();
        let handle = self.generate_handle_fn();
        let query = self.generate_query_fn();

        quote! {
            #struct_impl
            #init_msg
            #handle_msg
            #query_msg
            #query_response
            #init
            #handle
            #query
        }
    }

    fn generate_default_impl(&self) -> TokenStream {
        let struct_ident = Ident::new(DEFAULT_IMPL_STRUCT, Span::call_site());
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
                fields: Fields::Named(fields),
                discriminant: None
            });
        }

        match msg_type {
            MsgType::Handle => {
                for component in self.args.handle_components() {
                    result.variants.push(component.create_enum_variant(HANDLE_MSG));
                }
            },
            MsgType::Query => {
                for component in self.args.query_components() {
                    result.variants.push(component.create_enum_variant(QUERY_MSG));
                }
            }
        }
        
        result
    }

    fn generate_response_msg(&self) -> ItemEnum {
        let enum_name = Ident::new(RESPONSE_MSG, Span::call_site());

        let mut result: ItemEnum = parse_quote!{
            #[derive(serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
            #[serde(rename_all = "snake_case")]
            pub enum #enum_name {

            }
        };

        for method in &self.query {
            let variant_name = to_pascal(&method.sig.ident.to_string());
            let mut field_name = None;

            for attr in &method.attrs {
                if let Ok(meta) = attr.parse_meta() {
                    if let Meta::List(meta_list) = meta {
                        assert!(meta_list.nested.len() == 1, "Expected 1 string literal argument in \"query\" attribute.");

                        if let NestedMeta::Lit(lit) = meta_list.nested.first().unwrap() {
                            if let Lit::Str(name) = lit {
                                field_name = Some(name.clone());
                            }
                        }
                    }
                }
            }

            let field_name = field_name.expect("Expected 1 string literal argument in \"query\" attribute.");
            let field_name = Ident::new(&field_name.value(), Span::call_site());
            let result_ty = extract_std_result_type(&method.sig.output);

            result.variants.push(Variant {
                attrs: vec![],
                ident: Ident::new(&variant_name, Span::call_site()),
                fields: Fields::Named(parse_quote!({ #field_name: #result_ty })),
                discriminant: None
            });
        }

        for component in self.args.query_components() {
            result.variants.push(component.create_enum_variant(RESPONSE_MSG));
        }

        result
    }

    fn generate_init_msg(&self) -> TokenStream {
        if let Some(init) = &self.init {
            let msg = Ident::new(INIT_MSG, Span::call_site());

            let mut result: ItemStruct = parse_quote!{
                #[derive(serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
                pub struct #msg {
    
                }
            };
    
            let fields = extract_fields(&init);
            result.fields = Fields::Named(fields);
    
            return quote!(#result);
        }

        TokenStream::new()
    }
    
    fn generate_init_fn(&self) -> TokenStream {
        if let Some(init) = &self.init {
            let msg = Ident::new(INIT_MSG, Span::call_site());
            let fn_name = Ident::new(INIT_FN, Span::call_site());
            let arg = self.create_trait_arg();
    
            let mut result: ItemFn = parse_quote! {
                pub fn #fn_name<S: cosmwasm_std::Storage, A: cosmwasm_std::Api, Q: cosmwasm_std::Querier>(
                    deps: &mut cosmwasm_std::Extern<S, A, Q>,
                    env: cosmwasm_std::Env,
                    msg: #msg,
                    #arg
                ) -> StdResult<InitResponse> { }
            };
    
            let ref method_name = init.sig.ident;
    
            let mut args = Punctuated::<ExprField, Comma>::new();
    
            for input in &init.sig.inputs {
                let ident = extract_fn_arg_ident(input);
                args.push_value(parse_quote!(msg.#ident));
                args.push_punct(Comma(Span::call_site()));
            }
    
            let arg_name = Ident::new(CONTRACT_ARG, Span::call_site());
    
            let call: Expr = parse_quote!(#arg_name.#method_name(#args deps, env));
            result.block.stmts.push(Stmt::Expr(call));
    
            return quote!(#result);
        }

        TokenStream::new()
    }
    
    fn generate_handle_fn(&self) -> ItemFn {
        let msg = MsgType::Handle.to_ident();
        let arg = self.create_trait_arg();
        let fn_name = Ident::new(HANDLE_FN, Span::call_site());

        let mut result: ItemFn = parse_quote! {
            pub fn #fn_name<S: cosmwasm_std::Storage, A: cosmwasm_std::Api, Q: cosmwasm_std::Querier>(
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
        let fn_name = Ident::new(QUERY_FN, Span::call_site());

        let mut result: ItemFn = parse_quote! {
            pub fn #fn_name<S: cosmwasm_std::Storage, A: cosmwasm_std::Api, Q: cosmwasm_std::Querier>(
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

        match msg_type {
            MsgType::Handle => {
                for component in self.args.handle_components() {
                    let mod_name = component.mod_ident(true);
                    let ref mod_path = component.path;
                    let impl_struct = component.create_impl_struct();
                    let handle_fn = Ident::new(HANDLE_FN, Span::call_site());

                    match_expr.arms.push(
                        parse_quote!(#enum_name::#mod_name(msg) => #mod_path::#handle_fn(deps, env, msg, #impl_struct))
                    );
                }
            },
            MsgType::Query => {
                for component in self.args.query_components() {
                    let mod_name = component.mod_ident(true);
                    let ref mod_path = component.path;
                    let impl_struct = component.create_impl_struct();
                    let query_fn = Ident::new(QUERY_FN, Span::call_site());

                    match_expr.arms.push(
                        parse_quote!(#enum_name::#mod_name(msg) => #mod_path::#query_fn(deps, msg, #impl_struct))
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

fn extract_fields(method: &TraitItemMethod) -> FieldsNamed {
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

    fields
}

fn validate_method(method: &TraitItemMethod, expected: Option<Path>, contract_type: ContractType) {
    match contract_type {
        ContractType::Interface => {
            assert!(method.default.is_none(), "Contract interface method cannot contain a default implementation: \"{}\".", method.sig.ident);
        }
        _ => {
            assert!(method.default.is_some(), "Contract method must contain a default implementation: \"{}\".", method.sig.ident);
        }
    }

    let result_ty = extract_std_result_type(&method.sig.output);

    if let Some(path) = &expected {
        let ref generic_ident = result_ty.path.segments.last().unwrap().ident;
        let ref expected = path.segments.last().unwrap().ident;
        
        if *generic_ident != *expected {
            let expected_type = format!("{}", quote!{ #expected });
            panic!("Expecting return type: StdResult<{}>", expected_type);
        }
    }
}

fn extract_std_result_type(return_ty: &ReturnType) -> &TypePath {
    if let ReturnType::Type(_, return_type) = return_ty {
        if let Type::Path(return_type_path) = return_type.as_ref() {
            assert!(return_type_path.qself.is_none(), "Unexpected \"Self\" in return type.");

            let last = return_type_path.path.segments.last().unwrap();
            
            if last.ident.to_string().as_str() == "StdResult" {
                if let PathArguments::AngleBracketed(args) = &last.arguments {
                    if let GenericArgument::Type(ty) =  &args.args[0] {
                        if let Type::Path(generic_path) = ty {
                            return generic_path;
                        }
                    }
                }
            }
        }
    }

    panic!("Expecting return type: StdResult<T>")
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
