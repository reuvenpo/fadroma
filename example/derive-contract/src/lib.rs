use std::vec;

use scrt_derive_contract::{init, handle, query, contract};
use fadroma::scrt::{
    addr::Canonize,
    cosmwasm_std,
    cosmwasm_std::{
        Api, Extern, HandleResponse, HumanAddr, InitResponse,
        Querier, StdError, StdResult, Storage
    },
    storage::{load, ns_load, ns_save, save}
};
use serde::{Serialize, Deserialize};
use schemars::JsonSchema;

const NS_VOTERS: &[u8] = b"voters";
const KEY_VOTES: &[u8] = b"votes";

#[contract(Response)]
pub trait Votes {
    #[init]
    fn new(options: Vec<String>) -> StdResult<InitResponse> {
        let options = options.into_iter().map(|x| VoteOption::new(x)).collect();
        save_votes(&mut deps.storage, &options)?;

        Ok(InitResponse::default())
    }

    #[handle]
    fn vote(option: String) -> StdResult<HandleResponse> {
        if has_voted(&deps, &env.message.sender)? {
            return Err(StdError::generic_err("Already voted"))
        }

        let mut votes = load_votes(&deps.storage)?;
        let index = votes.iter().position(|x| x.description == option);

        if let Some(i) = index {
            votes[i].votes += 1;
            save_votes(&mut deps.storage, &votes)?;

            save_voter_addr(deps, &env.message.sender)?;
        } else {
            return Err(StdError::generic_err("Option not found"));
        }

        Ok(HandleResponse::default())
    }

    #[query]
    fn status() -> StdResult<Response> {
        let votes = load_votes(&deps.storage)?;

        Ok(Response {
            votes
        })
    }
}

#[derive(Serialize, Deserialize, JsonSchema)]
pub struct Response {
    votes: Vec<VoteOption>
}

#[derive(Serialize, Deserialize, JsonSchema, Debug, PartialEq)]
pub struct VoteOption {
    pub description: String,
    pub votes: u32
}

impl VoteOption {
    fn new(description: String) -> Self {
        Self {
            description,
            votes: 0
        }
    }
}

fn load_votes(storage: &impl Storage) -> StdResult<Vec<VoteOption>> {
    let votes: Option<Vec<VoteOption>> = load(storage, KEY_VOTES)?;
    
    Ok(votes.unwrap())
}

fn save_votes(storage: &mut impl Storage, votes: &Vec<VoteOption>) -> StdResult<()> {
    save(storage, KEY_VOTES, votes)
}

fn save_voter_addr<S: Storage, A: Api, Q: Querier>(
    deps: &mut Extern<S,A,Q>,
    address: &HumanAddr
) -> StdResult<()> {
    let address = address.canonize(&deps.api)?;

    ns_save(&mut deps.storage, NS_VOTERS, address.as_slice(), &true)
}

fn has_voted<S: Storage, A: Api, Q: Querier>(
    deps: &Extern<S,A,Q>,
    address: &HumanAddr
) -> StdResult<bool> {
    let address = address.canonize(&deps.api)?;
    let result: Option<bool> = ns_load(&deps.storage, NS_VOTERS, address.as_slice())?;
    
    Ok(result.is_some())
}

#[cfg(test)]
mod test {
    use super::*;
    use fadroma::scrt::cosmwasm_std::Extern;
    use fadroma::scrt::cosmwasm_std::from_binary;
    use fadroma::scrt::cosmwasm_std::testing::{mock_dependencies, mock_env, MockApi, MockQuerier, MockStorage};

    /// Query for right amount of votes
    fn assert_query_ok(
        deps: &mut Extern<MockStorage, MockApi, MockQuerier>,
        option1: u32,
        option2: u32,
    ) {
        let compare_votes = vec![
            VoteOption {
                description: "option1".to_string(),
                votes: option1
            },
            VoteOption {
                description: "option2".to_string(),
                votes: option2
            }
        ];
        let query_response = query(deps, QueryMsg::Status {}).unwrap();
        let res: Response = from_binary(&query_response).unwrap();

        assert_eq!(res.votes, compare_votes);
    }

    #[test]
    fn init_vote_and_query() {
        let mut deps = mock_dependencies(1000, &[]);

        let mut env = mock_env("creator", &[]);
        env.block.height = 876;

        let res = init(
            &mut deps,
            env,
            InitMsg {
                options: vec![String::from("option1"), String::from("option2")],
            },
        );

        // assert init didn't run into any trouble
        assert!(res.is_ok());

        // assert we can query the state and no votes are present and empty
        assert_query_ok(&mut deps, 0, 0);

        let mut env = mock_env("voter", &[]);
        env.block.height = 876;

        handle(
            &mut deps,
            env,
            HandleMsg::Vote {
                option: "option1".to_string(),
            },
        ).unwrap();

        // assert vote has been recorded properly
        assert_query_ok(&mut deps, 1, 0);

        let mut env = mock_env("voter", &[]);
        env.block.height = 876;
        let handle_res = handle(
            &mut deps,
            env,
            HandleMsg::Vote {
                option: "option1".to_string(),
            },
        );

        // assert we get error, since the vote cannot be cast twice by the same voter
        assert_eq!(
            handle_res,
            Err(StdError::GenericErr {
                msg: "Already voted".to_string(),
                backtrace: None
            })
        );

        let mut env = mock_env("voter1", &[]);
        env.block.height = 876;
        let handle_res = handle(
            &mut deps,
            env,
            HandleMsg::Vote {
                option: "option3".to_string(),
            },
        );

        // assert we get error, since we tried voting for option that doesn't exist
        assert_eq!(
            handle_res,
            Err(StdError::GenericErr {
                msg: "Option not found".to_string(),
                backtrace: None
            })
        );

        let mut env = mock_env("voter1", &[]);
        env.block.height = 876;
        handle(
            &mut deps,
            env,
            HandleMsg::Vote {
                option: "option2".to_string(),
            },
        ).unwrap();

        // assert voting went okay, and the proper votes are recorded
        assert_query_ok(&mut deps, 1, 1);
    }
}
