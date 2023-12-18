use std::path::PathBuf;

pub const CONFIG_KEY: &str = "nil";

#[derive(Debug, Clone)]
pub struct Config {
    pub root_path: PathBuf,

    pub gleam_binary: PathBuf,
}

impl Config {
    pub fn new(root_path: PathBuf) -> Self {
        assert!(root_path.is_absolute());
        Self {
            root_path,
            gleam_binary: "gleam".into(),
        }
    }

    // TODO: Simplify.
    pub fn update(&mut self, mut value: serde_json::Value) -> Vec<String> {
        let mut errors = Vec::new();

        if let Some(v) = value.pointer_mut("/gleam/binary") {
            match serde_json::from_value::<PathBuf>(v.take()) {
                Ok(path) => {
                    self.gleam_binary = path;
                }
                Err(e) => {
                    errors.push(format!("Invalid value of `gleam.binary`: {e}"));
                }
            }
        }

        errors
    }
}
