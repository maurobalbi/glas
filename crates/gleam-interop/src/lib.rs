//! Wrapper for `gleam`.
use anyhow::{anyhow, Context, Result};
use std::{path::PathBuf, process::{Command, Stdio}};

pub fn load_package_info(p: &PathBuf) -> Result<()> {
    let output = Command::new("gleam")
        .current_dir(p.parent().context("No parent")?)
        .args(["deps", "download"])
        .stdin(Stdio::null())
        // Configures stdout/stderr automatically.
        .output()
        .with_context(|| "Failed to download dependencies".to_string())?;

    if !output.status.success() {
        let err_msg = String::from_utf8(output.stderr)?.to_owned();
        return Err(anyhow!(format!(
            "The gleam binary had this to say: {}",
            err_msg
        )));
    }
    Ok(())
}
