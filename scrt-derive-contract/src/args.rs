use syn::{Path, NestedMeta, Meta, AttributeArgs, Lit, PathArguments};
use syn::token::Comma;
use syn::punctuated::Punctuated;

pub struct ContractArgs {
    pub response: Path,
    pub components: Vec<Component>
}

pub struct Component {
    pub path: Path,
    pub custom_impl: Option<Path>,
    pub skip_init: bool,
    pub skip_handle: bool,
    pub skip_query: bool,
}

impl ContractArgs {
    pub fn parse(args: AttributeArgs) -> Self {
        assert!(args.len() > 0, "Expected at least \"response\" argument in \"contract\" attribute.");

        let mut response = None;
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
                    Meta::NameValue(name_val) => {
                        let name = extract_path_ident_name(&name_val.path);

                        if name == "response" {
                            assert!(response.is_none(), "Duplicate \"response\" argument.");
                            
                            if let Lit::Str(val) = name_val.lit {
                                response = Some(val.parse().unwrap());
                            } else {
                                panic!("Expected string literal with impl type.")
                            }
                        } else {
                            panic!("Unexpected attribute: \"{}\"", name);
                        }
                    }
                    _ => panic!("Unexpected meta value in \"contract\" attribute.")
                }
            } else {
                panic!("Unexpected literal in \"contract\" attribute.");
            }
        }

        Self {
            response: response.expect("Expected \"response\" argument in \"contract\" attribute."),
            components
        }
    }
}

impl Component {
    pub fn parse(nested: Punctuated<NestedMeta, Comma>) -> Self {
        let mut skip_init = false;
        let mut skip_handle = false;
        let mut skip_query = false;
        let mut custom_impl = None;
        let mut path = None;

        for entry in nested {
            match entry {
                NestedMeta::Meta(meta) => {
                    match meta {
                        Meta::NameValue(name_val) => {
                            let name = extract_path_ident_name(&name_val.path);

                            match name.as_str() {
                                "path" => {
                                    if let Lit::Str(val) = name_val.lit {
                                        path = Some(val.parse().unwrap());
                                    }
                                },
                                "custom_impl" => {
                                    if let Lit::Str(val) = name_val.lit {
                                        custom_impl = Some(val.parse().unwrap());
                                    } else {
                                        panic!("Expected string literal with impl type.")
                                    }
                                },
                                _ => panic!("Unexpected attribute: \"{}\"", name)
                            }
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

        Self {
            path: path.expect("Module \"path\" argument expected."),
            custom_impl,
            skip_init,
            skip_handle,
            skip_query
        }
    }
}

fn extract_path_ident_name(path: &Path) -> String {
    assert!(path.segments.len() == 1);
    let name = path.segments.first().unwrap();
    assert!(name.arguments == PathArguments::None);

    name.ident.to_string()
}
