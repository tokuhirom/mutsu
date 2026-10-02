//! The program name a `MAIN` usage line starts with (Rakudo's
//! `default-generate-usage` prelude in `src/core.c/Main.rakumod`).

use super::*;

impl Interpreter {
    /// The program name a usage line starts with: `-e '...'` for a one-liner,
    /// the bare name for a script found through `PATH`, else `$*PROGRAM-NAME`
    /// as given. `%*ENV<PERL6_PROGRAM_NAME>` overrides it.
    pub(super) fn usage_program_name(&self) -> String {
        let name = std::env::var("PERL6_PROGRAM_NAME")
            .ok()
            .filter(|s| !s.is_empty())
            .or_else(|| {
                self.env
                    .get("*PROGRAM-NAME")
                    .or_else(|| self.env.get("$*PROGRAM-NAME"))
                    .map(|v| v.to_string_value())
            })
            .unwrap_or_else(|| "program".to_string());
        if name == "-e" {
            "-e '...'".to_string()
        } else {
            strip_path_prefix(&name)
        }
    }
}

/// Rakudo's `strip_path_prefix`: a script run through `PATH` is named by its
/// base name (or by its installed wrapper's name), unless an earlier `PATH`
/// entry shadows it; any other name is kept as given.
pub(super) fn strip_path_prefix(name: &str) -> String {
    use std::path::Path;
    let path = Path::new(name);
    let Some(base) = path.file_name().and_then(|b| b.to_str()) else {
        return name.to_string();
    };
    let dir = path.parent().map(Path::to_path_buf).unwrap_or_default();
    let canon = |p: &Path| std::fs::canonicalize(p).ok();
    let dir_canon = canon(if dir.as_os_str().is_empty() {
        Path::new(".")
    } else {
        &dir
    });
    let Some(path_var) = std::env::var_os("PATH") else {
        return name.to_string();
    };
    let elems: Vec<std::path::PathBuf> = std::env::split_paths(&path_var).collect();
    for elem in &elems {
        let file = elem.join(base);
        if is_executable_file(&file) {
            return if canon(elem) == dir_canon {
                base.to_string()
            } else {
                name.to_string()
            };
        }
    }
    if let Some(wrapper_base) = base.strip_suffix(".raku") {
        for elem in &elems {
            let wrapper = elem.join(wrapper_base);
            if is_executable_file(&wrapper) && elem.join(base).is_file() {
                return if canon(elem) == dir_canon {
                    wrapper_base.to_string()
                } else {
                    name.to_string()
                };
            }
        }
    }
    name.to_string()
}

fn is_executable_file(path: &std::path::Path) -> bool {
    let Ok(meta) = std::fs::metadata(path) else {
        return false;
    };
    if !meta.is_file() {
        return false;
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        meta.permissions().mode() & 0o111 != 0
    }
    #[cfg(not(unix))]
    {
        true
    }
}
