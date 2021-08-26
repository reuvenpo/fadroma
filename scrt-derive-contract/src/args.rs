use std::collections::HashMap;

use syn::{
    Path, NestedMeta, Meta, AttributeArgs,
    Lit, LitStr, PathArguments, MetaNameValue,
    Ident, Variant, parse_quote
};
use syn::token::Comma;
use syn::punctuated::Punctuated;
use syn::parse::Parse;
use proc_macro2::Span;

use crate::contract::DEFAULT_IMPL_STRUCT;
use crate::utils::to_pascal;

pub struct ContractArgs {
    components: Vec<Component>
}

pub struct Component {
    pub path: Path,
    custom_impl: Option<Path>,
    skip_init: bool,
    skip_handle: bool,
    skip_query: bool,
}

struct MetaNameValueParser {
    entries: HashMap<String, LitStr>
}

impl ContractArgs {
    pub fn parse(args: AttributeArgs) -> Self {
        let mut components = vec![];

        for arg in args {
            if let NestedMeta::Meta(meta) = arg {
                match meta {
                    Meta::List(list) => {
                        let segment = list.path.segments.first().expect("Expected path identifier.");
                        let name = segment.ident.to_string();

                        if name == "component" {
                            components.push(Component::parse(list.nested))
                        } else {
                            panic!("Unexpected attribute: \"{}\"", name);
                        }
                    },
                    _ => panic!("Unexpected meta value in \"contract\" attribute.")
                }
            } else {
                panic!("Unexpected literal in \"contract\" attribute.");
            }
        }

        Self {
            components
        }
    }

    pub fn init_components(&self) -> impl Iterator<Item = &Component> {
        self.components.iter().filter(|x| !x.skip_init)
    }

    pub fn handle_components(&self) -> impl Iterator<Item = &Component> {
        self.components.iter().filter(|x| !x.skip_handle)
    }

    pub fn query_components(&self) -> impl Iterator<Item = &Component> {
        self.components.iter().filter(|x| !x.skip_query)
    }
}

impl Component {
    pub fn parse(nested: Punctuated<NestedMeta, Comma>) -> Self {
        let mut skip_init = false;
        let mut skip_handle = false;
        let mut skip_query = false;

        let mut parser = MetaNameValueParser::new();

        for entry in nested {
            match entry {
                NestedMeta::Meta(meta) => {
                    match meta {
                        Meta::NameValue(name_val) => {
                            parser.parse(name_val);
                        },
                        Meta::List(list) => {
                            let name = extract_path_ident_name(&list.path);

                            match name.as_str() {
                                "skip" => {
                                    for arg in list.nested {
                                        if let NestedMeta::Meta(meta) = arg {
                                            if let Meta::Path(skip_arg) = meta {
                                                let skipable = extract_path_ident_name(&skip_arg);

                                                match skipable.as_str() {
                                                    "init" => skip_init = true,
                                                    "handle" => skip_handle = true,
                                                    "query" => skip_query = true,
                                                    _ => panic!("Unexpected argument in \"skip\" attribute: \"{}\"", skipable)
                                                }
                                            }
                                        }
                                    }
                                },
                                _ => panic!("Unexpected attribute: \"{}\"", name)
                            }
                        },
                        Meta::Path(_) => panic!("Unexpected meta path in attribute.")
                    }
                },
                NestedMeta::Lit(_) => panic!("Unexpected literal in attribute.")
            }
        }

        let path = parser.require("path");
        let custom_impl = parser.get("custom_impl");
        parser.finalize();

        Self {
            path,
            custom_impl,
            skip_init,
            skip_handle,
            skip_query
        }
    }

    pub fn path_concat(&self, ident: &Ident) -> Path {
        let ref path = self.path;

        parse_quote!(#path::#ident)
    }

    pub fn create_impl_struct(&self) -> Path {
        let ref path = self.path;

        if let Some(custom_impl) = &self.custom_impl {
            return parse_quote!(#path::#custom_impl);
        }

        let default = Ident::new(DEFAULT_IMPL_STRUCT, Span::call_site());
        parse_quote!(#path::#default)
    }

    pub fn create_enum_variant(&self, msg_name: &'static str) -> Variant {
        let mod_ident = self.mod_ident(true);
        let msg_path = self.path_concat(&Ident::new(msg_name, Span::call_site()));

        parse_quote!(#mod_ident(#msg_path))
    }

    pub fn mod_ident(&self, pascal_case: bool) -> Ident {
        let ident = self.path.segments.last().unwrap().ident.clone();

        if pascal_case {
            return Ident::new(&to_pascal(&ident.to_string()), Span::call_site());
        }

        ident
    }
}

impl MetaNameValueParser {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new()
        }
    }

    pub fn parse(&mut self, name_val: MetaNameValue) {
        let name = extract_path_ident_name(&name_val.path);

        match name_val.lit {
            Lit::Str(val) => {
                if self.entries.contains_key(&name) {
                    panic!("Duplicate \"{}\" attribute.", name)
                }

                self.entries.insert(name, val);
            },
            _ => panic!("Expected string literal for \"{}\".", name)
        }
    }

    pub fn require<T: Parse>(&mut self, name: &str) -> T {
        if let Some(value) = self.entries.remove(name) {
            value.parse().unwrap()
        } else {
            panic!("Expected attribute: \"{}\"", name)
        }
    }

    pub fn get<T: Parse>(&mut self, name: &str) -> Option<T> {
        if let Some(value) = self.entries.remove(name) {
            Some(value.parse().unwrap())
        } else {
            None
        }
    }

    pub fn finalize(mut self) {
        if self.entries.len() > 0 {
            let unexpected: Vec<String> = self.entries.drain().map(|x| x.0).collect();
            panic!("Unexpected atrributes: {}", unexpected.join(", "))
        }
    }
}

fn extract_path_ident_name(path: &Path) -> String {
    assert!(path.segments.len() == 1);
    let name = path.segments.first().unwrap();
    assert!(name.arguments == PathArguments::None);

    name.ident.to_string()
}
