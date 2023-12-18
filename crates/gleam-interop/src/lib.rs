//! Wrapper for `gleam`.
use std::process::Stdio;
use anyhow::{anyhow, Context, Result};
use tokio::process::Command;

pub async fn load_package_info() -> Result<()> {
    let output = Command::new("gleam")
        .kill_on_drop(true)
        .args(["deps", "download"])
        .stdin(Stdio::null())
        // Configures stdout/stderr automatically.
        .output()
        .await
        .with_context(|| "Failed to download dependencies".to_string())
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

