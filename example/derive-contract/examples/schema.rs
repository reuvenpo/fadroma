use std::env::current_dir;
use std::fs::create_dir_all;

use fadroma::scrt::cosmwasm_schema::{export_schema, remove_schemas, schema_for};

use derive_votes;

fn main() {
    let mut out_dir = current_dir().unwrap();
    out_dir.push("schema");
    create_dir_all(&out_dir).unwrap();
    remove_schemas(&out_dir).unwrap();

    export_schema(&schema_for!(derive_votes::InitMsg), &out_dir);
    export_schema(&schema_for!(derive_votes::QueryMsg), &out_dir);
    export_schema(&schema_for!(derive_votes::HandleMsg), &out_dir);
    export_schema(&schema_for!(derive_votes::Response), &out_dir);
}