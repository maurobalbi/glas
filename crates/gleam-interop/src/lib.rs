//! Wrapper for `gleam`.
use std::process::Stdio;
use std::{collections::HashMap, str};

use serde::Deserialize;

use anyhow::{anyhow, Context, Result};
use smol_str::SmolStr;
use tokio::process::Command;

pub async fn load_package_info() -> Result<()> {
    let output = Command::new("gleam")
        .kill_on_drop(true)
        .args(["deps", "download"])
        .stdin(Stdio::null())
        // Configures stdout/stderr automatically.
        .output()
        .await
        .with_context(|| format!("Failed to download dependencies"))
        .unwrap();

    if !output.status.success() {
        let err_msg = String::from_utf8(output.stderr)?.to_owned();
        return Err(anyhow!(format!(
            "The gleam binary had this to say: {}",
            err_msg
        )));
    }
    Ok(())
}

#[derive(Deserialize, Debug, PartialEq, Clone)]
pub struct ManifestToml {
    pub name: SmolStr,
    pub target: SmolStr,
    pub dependencies: HashMap<String, String>,
}
