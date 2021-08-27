use cosmwasm_std::{StdResult, InitResponse, HandleResponse, Storage, to_vec, from_slice};
use scrt_derive_contract::*;
use schemars;
use serde;

pub mod string_component {
    use super::*;

    const KEY_STRING: &[u8] = b"string_data";

    #[contract]
    pub trait StringComponent {
        fn new(storage: &mut impl Storage, string: String) -> StdResult<()> {
            Ok(storage.set(KEY_STRING, &to_vec(&string)?))
        }

        #[handle]
        fn set_string(string: String) -> StdResult<HandleResponse> {
            deps.storage.set(KEY_STRING, &to_vec(&string)?);

            Ok(HandleResponse::default())
        }

        #[query("string")]
        fn get_string() -> StdResult<String> {
            let value = deps.storage.get(KEY_STRING).unwrap();

            from_slice(&value)
        }
    }
}

pub mod number_interface {
    use super::*;

    #[interface(component(path = "string_component", skip(handle)))]
    pub trait NumberInterface {
        fn new(number: u8) -> StdResult<InitResponse>;

        #[handle]
        fn set_number(number: u8) -> StdResult<HandleResponse>;

        #[query("number")]
        fn get_number() -> StdResult<u8>;
    }
}

pub mod number_contract {
    use super::*;
    use string_component::StringComponent;

    const KEY_NUMBER: &[u8] = b"number_data";

    #[contract(component(path = "string_component", skip(handle)))]
    pub trait NumberContract {
        #[init]
        fn new(number: u8, string: String) -> StdResult<InitResponse> {
            string_component::DefaultImpl::new(&mut deps.storage, string)?;
            deps.storage.set(KEY_NUMBER, &to_vec(&number)?);

            Ok(InitResponse::default())
        }

        #[handle]
        fn set_number(number: u8) -> StdResult<HandleResponse> {
            deps.storage.set(KEY_NUMBER, &to_vec(&number)?);

            Ok(HandleResponse::default())
        }

        #[query("number")]
        fn get_number() -> StdResult<u8> {
            let value = deps.storage.get(KEY_NUMBER).unwrap();

            from_slice(&value)
        }
    }
}
