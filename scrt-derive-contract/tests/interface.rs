use cosmwasm_std::{StdResult, InitResponse, HandleResponse};
use scrt_derive_contract::*;
use schemars;
use serde;

pub mod test_interface {
    use super::*;

    #[interface(response = "TestInterfaceResponse",
        component(path = "test::module", skip(init, handle, query), custom_impl = "MyImpl")
    )]
    pub trait TestInterface {
        #[init]
        fn new(data: u8) -> StdResult<InitResponse>;

        #[handle]
        fn set_data(data: u8) -> StdResult<HandleResponse>;

        #[query]
        fn get_data() -> StdResult<TestInterfaceResponse>;
    }

    pub enum TestInterfaceResponse {
        GetData {
            data: u8
        }
    }
}
