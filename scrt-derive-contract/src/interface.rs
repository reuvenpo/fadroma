use syn::{
    TraitItemMethod, Path, NestedMeta, AttributeArgs, ItemTrait,
    Meta, TraitItem, AttrStyle, ReturnType, Type, Ident, ItemEnum,
    Variant, FnArg, FieldsNamed, Field, Visibility, Pat, Fields,
    ItemStruct, parse_quote
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

pub struct ContractInterface {
    ident: Ident,
    init: TraitItemMethod,
    handle: Vec<TraitItemMethod>,
    query: Vec<TraitItemMethod>
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
        let handle_msg = self.generate_messages("HandleMsg", &self.handle);
        let query_msg = self.generate_messages("QueryMsg", &self.query);

        quote! {
            #struct_impl
            #init_msg
            #handle_msg
            #query_msg
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

    fn generate_messages(&self, enum_name: &str, methods: &Vec<TraitItemMethod>) -> ItemEnum {
        let enum_name = Ident::new(enum_name, Span::call_site());

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
        let mut result: ItemStruct = parse_quote!{
            #[derive(serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
            pub struct InitMsg {

            }
        };

        result.fields = extract_fields(&self.init);

        result
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
