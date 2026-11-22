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

/// Hand the parser the `-M` modules for the *next* compilation unit it parses
/// — the mainline. Rakudo treats `-MFoo` as a `use Foo;` at the top of the
/// program, so their exports are in scope for the whole parse: an exported
/// `multi sub die` must shadow the `die` statement form exactly as it would
/// after a written `use`. The runtime loads the modules itself before running;
/// this only lets the parser see the names they import.
pub fn set_parser_preload_modules(modules: Vec<String>) {
    PRELOAD_MODULES.with(|p| *p.borrow_mut() = modules);
}

/// Register the exports of the pending `-M` modules into the current scope,
/// consuming the list so a nested parse (a module the program `use`s, an
/// `EVAL`) does not import them again.
pub(in crate::parser) fn register_preload_module_exports() {
    let modules = PRELOAD_MODULES.with(|p| std::mem::take(&mut *p.borrow_mut()));
    for module in &modules {
        register_module_exports(module);
    }
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

/// Mark the compilation unit about to be parsed as a module (or not), and
/// return the previous mark so the caller can restore it; module parses nest
/// like [`set_parser_source_file`].
///
/// Rakudo reports a module's compile-time worries once, when it precompiles
/// the module, and never on a later load; mutsu replays a module's parse
/// warnings on every load. A worry that can come in the hundreds for an
/// ordinary module (a "Missing declarand" per `#=`-documented enum value) is
/// therefore reported for the program's own units only.
// Cost: O(1).
pub fn set_parser_module_unit(module: bool) -> bool {
    MODULE_UNIT.with(|m| std::mem::replace(&mut *m.borrow_mut(), module))
}

/// Whether the compilation unit being parsed is a module.
// Cost: O(1).
pub(crate) fn parser_module_unit() -> bool {
    MODULE_UNIT.with(|m| *m.borrow())
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

/// The individual path arguments of a `use lib` argument expression, in source
/// order: a single path, a comma list (`'a', 'b'`, `<a b>`) or a parenthesised
/// one (`('a', 'b')`, `(('a'), 'b')`) — lists and parentheses are looked
/// through, everything else is a leaf. The one decoder for every consumer of
/// `use lib` (the parser's BEGIN-time search path, the compiler's nested-`use`
/// prologue, the runtime's pre-execution type scan); each applies its own
/// rule to the leaves (literal-only, or folded by [`fold_use_lib_path`]).
// Cost: O(n), n = number of list/paren nodes in the argument's spine.
pub(crate) fn use_lib_args(expr: &Expr) -> impl Iterator<Item = &Expr> {
    let mut stack = vec![expr];
    std::iter::from_fn(move || {
        while let Some(e) = stack.pop() {
            match e {
                Expr::ArrayLiteral(items) => stack.extend(items.iter().rev()),
                Expr::Grouped(inner) => stack.push(inner),
                leaf => return Some(leaf),
            }
        }
        None
    })
}

/// Try to extract a library path from a `use lib` expression at parse time.
/// Handles string literals and `$*PROGRAM.parent(N).add("path")` patterns.
pub(crate) fn try_add_parse_time_lib_path(expr: &Expr) {
    if let Some(path) = extract_lib_path(expr).map(absolute_parse_time_lib_path) {
        LIB_PATHS.with(|p| {
            let mut paths = p.borrow_mut();
            if !paths.contains(&path) {
                paths.push(path);
            }
        });
    }
}

/// A relative `use lib` directory, made absolute against the process's
/// working directory -- what the runtime's `use lib` does against `$*CWD`
/// (`Interpreter::absolute_lib_spec`), so the parser's module probes and the
/// unit's own loads name each module file by one path (#17690). A spec that
/// names a repository kind (`inst#...`) is kept as written.
// Cost: O(n), n = length of the path.
fn absolute_parse_time_lib_path(path: String) -> String {
    let pb = std::path::Path::new(&path);
    if path.contains('#') || pb.is_absolute() {
        return path;
    }
    let Ok(cwd) = std::env::current_dir() else {
        return path;
    };
    let clean: std::path::PathBuf = cwd
        .join(pb)
        .components()
        .filter(|c| !matches!(c, std::path::Component::CurDir))
        .collect();
    clean.to_string_lossy().into_owned()
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
    let program = PROGRAM_PATH.with(|p| p.borrow().clone());
    let file = parser_source_file().or_else(|| program.clone());
    fold_use_lib_path(expr, file.as_deref(), program.as_deref())
}

/// Fold one `use lib` argument to its path, given the unit's `$?FILE` and the
/// running `$*PROGRAM`. Shared by the parser (which must import the module a
/// later `use` names while still parsing) and the runtime's pre-execution
/// type check (`eval_check`), so both agree on which directories a unit adds.
pub(crate) fn fold_use_lib_path(
    expr: &Expr,
    file: Option<&str>,
    program: Option<&str>,
) -> Option<String> {
    static_path(expr, file, program)
}

/// Fold a path-method chain rooted at `$?FILE`, `$*PROGRAM` or a string
/// literal to its path string (see [`extract_lib_path`]).
fn static_path(expr: &Expr, file: Option<&str>, program: Option<&str>) -> Option<String> {
    match expr {
        // The parser folds `$?FILE` itself to a string literal when it knows
        // the unit's file, so a literal is a chain root too (`'t'.IO.add(..)`).
        Expr::Literal(lit) => lit.as_str().map(|s| s.to_string()),
        Expr::Var(v) if v == "*PROGRAM" => program.map(str::to_string),
        Expr::Var(v) if v == "?FILE" => file.or(program).map(str::to_string),
        // A parenthesised chain link: `($?FILE.IO.parent).add('lib')`.
        Expr::Grouped(inner) => static_path(inner, file, program),
        // A `.AST` parse keeps the written `$?FILE` beside its folded literal.
        Expr::Spelled(spelled) => static_path(&spelled.expr, file, program),
        Expr::MethodCall {
            target, name, args, ..
        } => {
            let base = static_path(target, file, program)?;
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
                    Some(
                        std::path::Path::new(&base)
                            .join(&arg)
                            .to_string_lossy()
                            .into_owned(),
                    )
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
