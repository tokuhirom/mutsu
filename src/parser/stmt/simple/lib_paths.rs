use super::*;

/// Set the library search paths for the parser (called before parsing).
pub fn set_parser_lib_paths(paths: Vec<String>) {
    LIB_PATHS.with(|p| {
        *p.borrow_mut() = paths;
    });
}

/// Set the program path for module resolution relative to the script.
pub fn set_parser_program_path(path: Option<String>) {
    PROGRAM_PATH.with(|p| {
        *p.borrow_mut() = path;
    });
}

/// Clear the library search paths (called after parsing).
pub fn clear_parser_lib_paths() {
    LIB_PATHS.with(|p| {
        p.borrow_mut().clear();
    });
    LOADING_MODULES.with(|m| {
        m.borrow_mut().clear();
    });
    PROGRAM_PATH.with(|p| {
        *p.borrow_mut() = None;
    });
}

/// Try to extract a library path from a `use lib` expression at parse time.
/// Handles string literals and `$*PROGRAM.parent(N).add("path")` patterns.
pub(crate) fn try_add_parse_time_lib_path(expr: &Expr) {
    if let Some(path) = extract_lib_path(expr) {
        LIB_PATHS.with(|p| {
            let mut paths = p.borrow_mut();
            if !paths.contains(&path) {
                paths.push(path);
            }
        });
    }
}

/// Extract a concrete path from a `use lib` expression.
fn extract_lib_path(expr: &Expr) -> Option<String> {
    match expr {
        // use lib "some/path"
        Expr::Literal(Value::Str(s)) => Some(s.to_string()),
        // use lib $*PROGRAM.parent(N).add("path") or .add($*SPEC.catdir(<...>))
        Expr::MethodCall {
            target, name, args, ..
        } if name == "add" || name == "child" => {
            // Extract the string argument to .add()
            let add_arg = args.first().and_then(extract_static_string)?;
            // Resolve the target chain ($*PROGRAM.parent(N))
            let base = extract_program_parent(target)?;
            let result = std::path::Path::new(&base).join(&add_arg);
            Some(result.to_string_lossy().into_owned())
        }
        _ => None,
    }
}

/// Try to statically evaluate an expression to a string.
/// Handles string literals and `$*SPEC.catdir(<word list>)`.
fn extract_static_string(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Literal(Value::Str(s)) => Some(s.to_string()),
        // $*SPEC.catdir(<packages Test-Helpers lib>) → "packages/Test-Helpers/lib"
        Expr::MethodCall {
            target, name, args, ..
        } if name == "catdir" || name == "catfile" => {
            // Target should be $*SPEC
            if let Expr::Var(v) = target.as_ref()
                && v == "*SPEC"
            {
                let parts: Vec<String> = args
                    .iter()
                    .filter_map(|a| match a {
                        Expr::Literal(Value::Str(s)) => Some(s.to_string()),
                        Expr::ArrayLiteral(items) => {
                            let strs: Vec<String> = items
                                .iter()
                                .filter_map(|i| {
                                    if let Expr::Literal(Value::Str(s)) = i {
                                        Some(s.to_string())
                                    } else {
                                        None
                                    }
                                })
                                .collect();
                            if strs.is_empty() {
                                None
                            } else {
                                Some(strs.join("/"))
                            }
                        }
                        _ => None,
                    })
                    .collect();
                if parts.is_empty() {
                    return None;
                }
                return Some(parts.join("/"));
            }
            None
        }
        _ => None,
    }
}

/// Extract a directory path from `$*PROGRAM.parent(N)` or `$*PROGRAM.parent`.
fn extract_program_parent(expr: &Expr) -> Option<String> {
    match expr {
        Expr::MethodCall {
            target, name, args, ..
        } if name == "parent" => {
            // Get the base: should be $*PROGRAM or a chain
            let base = match target.as_ref() {
                Expr::Var(v) if v == "*PROGRAM" => PROGRAM_PATH.with(|p| p.borrow().clone())?,
                other => extract_program_parent(other)?,
            };
            let levels = if let Some(Expr::Literal(Value::Int(n))) = args.first() {
                *n as usize
            } else {
                1
            };
            let mut path_str = base;
            for _ in 0..levels {
                if path_str == "." {
                    path_str = "..".to_string();
                } else if path_str == ".." || path_str.ends_with("/..") {
                    path_str = format!("{}/..", path_str);
                } else if path_str == "/" {
                    break;
                } else if let Some(par) = std::path::Path::new(&path_str).parent() {
                    let s = par.to_string_lossy().to_string();
                    if s.is_empty() {
                        path_str = ".".to_string();
                    } else {
                        path_str = s;
                    }
                } else {
                    path_str = ".".to_string();
                }
            }
            Some(path_str)
        }
        Expr::MethodCall { target, name, .. } if name == "IO" => {
            // .IO is a no-op for path resolution
            match target.as_ref() {
                Expr::Var(v) if v == "*PROGRAM" => PROGRAM_PATH.with(|p| p.borrow().clone()),
                _ => None,
            }
        }
        Expr::Var(v) if v == "*PROGRAM" => PROGRAM_PATH.with(|p| p.borrow().clone()),
        _ => None,
    }
}
