use super::*;

impl Interpreter {
    pub(crate) fn resolved_current_executable_path() -> std::path::PathBuf {
        std::env::current_exe()
            .map(|path| {
                let is_cargo_test_binary = path
                    .parent()
                    .and_then(|parent| parent.file_name())
                    .is_some_and(|name| name == "deps")
                    && path
                        .file_stem()
                        .and_then(|stem| stem.to_str())
                        .is_some_and(|stem| stem.starts_with("mutsu-"));
                if is_cargo_test_binary
                    && let Some(target_dir) = path.parent().and_then(|parent| parent.parent())
                {
                    let sibling = target_dir.join("mutsu");
                    if sibling.is_file() {
                        return sibling;
                    }
                }
                path
            })
            .unwrap_or_else(|_| std::path::PathBuf::from("target/debug/mutsu"))
    }

    pub(super) fn dynamic_name_alias(name: &str) -> Option<String> {
        if let Some(rest) = name.strip_prefix("$*") {
            return Some(format!("*{}", rest));
        }
        if let Some(rest) = name.strip_prefix('*') {
            return Some(format!("$*{}", rest));
        }
        None
    }
}
