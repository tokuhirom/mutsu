use super::*;

/// Set the library search paths for the parser (called before parsing).
pub fn set_parser_lib_paths(paths: Vec<String>) {
    LIB_PATHS.with(|p| {
        *p.borrow_mut() = paths;
    });
}

/// The parser's current module search paths (runtime lib paths + bundled
/// batteries, plus any parse-time `use lib` additions).
pub(in crate::parser) fn parser_lib_paths() -> Vec<String> {
    LIB_PATHS.with(|p| p.borrow().clone())
}

/// Set the program path for module resolution relative to the script.
pub fn set_parser_program_path(path: Option<String>) {
    PROGRAM_PATH.with(|p| {
        *p.borrow_mut() = path;
    });
}

/// Install the file the compilation unit about to be parsed came from, and
/// return the previous one so the caller can restore it. Module parses nest
/// (a module's `use` triggers another `load_module` mid-parse), so this is a
/// swap rather than a plain set.
pub fn set_parser_source_file(path: Option<String>) -> Option<String> {
    SOURCE_FILE.with(|p| std::mem::replace(&mut *p.borrow_mut(), path))
}

/// The file `$?FILE` folds to for the compilation unit being parsed, if known.
pub(crate) fn parser_source_file() -> Option<String> {
    SOURCE_FILE.with(|p| p.borrow().clone())
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
///
/// `use lib` runs at BEGIN time, so its argument must be known while the rest
/// of the unit is still being parsed (a module it makes loadable can export
/// types a later signature names). The argument is folded statically: string
/// literals, and path-method chains rooted at a compile-time path —
/// `$?FILE` (the unit's own file) or `$*PROGRAM` — through `.IO`, `.Str`,
/// `.parent(N)`, `.add`/`.child` and `.sibling`.
fn extract_lib_path(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Literal(lit) => lit.as_str().map(|s| s.to_string()),
        _ => static_path(expr),
    }
}

/// Fold a path-method chain rooted at `$?FILE` or `$*PROGRAM` to its path
/// string (see [`extract_lib_path`]).
fn static_path(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Var(v) if v == "*PROGRAM" => PROGRAM_PATH.with(|p| p.borrow().clone()),
        Expr::Var(v) if v == "?FILE" => {
            parser_source_file().or_else(|| PROGRAM_PATH.with(|p| p.borrow().clone()))
        }
        Expr::MethodCall {
            target, name, args, ..
        } => {
            let base = static_path(target)?;
            match name.as_str() {
                // Coercions between Str and IO::Path leave the path unchanged.
                "IO" | "Str" if args.is_empty() => Some(base),
                "parent" => {
                    let levels = match args.first() {
                        None => 1,
                        Some(Expr::Literal(lit)) => usize::try_from(lit.as_int()?).ok()?,
                        Some(_) => return None,
                    };
                    Some(path_parent(base, levels))
                }
                "add" | "child" => {
                    let arg = args.first().and_then(extract_static_string)?;
                    Some(std::path::Path::new(&base).join(&arg).to_string_lossy().into_owned())
                }
                "sibling" => {
                    let arg = args.first().and_then(extract_static_string)?;
                    let dir = std::path::Path::new(&base)
                        .parent()
                        .unwrap_or_else(|| std::path::Path::new(""));
                    Some(dir.join(&arg).to_string_lossy().into_owned())
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// `IO::Path.parent(levels)` on a path string: a relative path climbs past
/// `.` into `..` rather than collapsing to an empty string.
fn path_parent(mut path_str: String, levels: usize) -> String {
    for _ in 0..levels {
        if path_str == "." {
            path_str = "..".to_string();
        } else if path_str == ".." || path_str.ends_with("/..") {
            path_str = format!("{}/..", path_str);
        } else if path_str == "/" {
            break;
        } else if let Some(par) = std::path::Path::new(&path_str).parent() {
            let s = par.to_string_lossy().to_string();
            path_str = if s.is_empty() { ".".to_string() } else { s };
        } else {
            path_str = ".".to_string();
        }
    }
    path_str
}

/// Try to statically evaluate an expression to a string.
/// Handles string literals and `$*SPEC.catdir(<word list>)`.
fn extract_static_string(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Literal(lit) => lit.as_str().map(|s| s.to_string()),
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
                        Expr::Literal(lit) => lit.as_str().map(|s| s.to_string()),
                        Expr::ArrayLiteral(items) => {
                            let strs: Vec<String> = items
                                .iter()
                                .filter_map(|i| {
                                    if let Expr::Literal(lit) = i
                                        && let Some(s) = lit.as_str()
                                    {
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
