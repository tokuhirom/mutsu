use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};

use regex::Regex;

use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{
    PError, PResult, merge_expected_messages, opt_char, parse_char, take_while1,
};

use crate::ast::{AssignOp, Expr, PhaserKind, Stmt};
use crate::value::Value;

/// A single lexical scope frame tracking both user-declared subs and module imports.
#[derive(Clone)]
enum TermBinding {
    Value(String),
    Callable(String),
}

#[derive(Clone, Default)]
struct LexicalScope {
    user_subs: HashSet<String>,
    infix_assoc: HashMap<String, String>,
    test_assertion_subs: HashSet<String>,
    imported_functions: HashSet<String>,
    term_symbols: HashMap<String, TermBinding>,
    /// Compile-time constants (name → string value) for resolving `<<$x>>` in operator names.
    compile_time_constants: HashMap<String, String>,
}

thread_local! {
    /// Lexical scope stack. Each `{ }` block pushes a new scope.
    /// `sub` declarations and `use` imports register names into the current (innermost) scope.
    /// Lookups search from innermost to outermost.
    static SCOPES: RefCell<Vec<LexicalScope>> = RefCell::new(vec![LexicalScope::default()]);

    /// Library search paths set before parsing, mirroring the runtime's lib_paths.
    static LIB_PATHS: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };

    /// Tracks which modules are currently being scanned to avoid infinite recursion.
    static LOADING_MODULES: RefCell<HashSet<String>> = RefCell::new(HashSet::new());

    /// Program file path, used to find modules relative to the script.
    static PROGRAM_PATH: RefCell<Option<String>> = const { RefCell::new(None) };

    /// Operator sub names to pre-register after scope reset (for EVAL).
    static EVAL_OPERATOR_PRESEED: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
    /// Infix associativity traits to pre-register after scope reset (for EVAL).
    static EVAL_OPERATOR_ASSOC_PRESEED: RefCell<HashMap<String, String>> =
        RefCell::new(HashMap::new());
}

static TMP_INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn parse_hyper_assign_op(input: &str) -> Option<&str> {
    const HYPER_ASSIGN_OPS: &[&str] = &[
        ">>=>>", "<<=<<", ">>=<<", "<<=>>", "»=»", "«=«", "»=«", "«=»",
    ];
    for op in HYPER_ASSIGN_OPS {
        if let Some(rest) = input.strip_prefix(op) {
            return Some(rest);
        }
    }
    None
}

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
pub(in crate::parser) fn try_add_parse_time_lib_path(expr: &Expr) {
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
        Expr::Literal(Value::Str(s)) => Some(s.clone()),
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
        Expr::Literal(Value::Str(s)) => Some(s.clone()),
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
                        Expr::Literal(Value::Str(s)) => Some(s.clone()),
                        Expr::ArrayLiteral(items) => {
                            let strs: Vec<String> = items
                                .iter()
                                .filter_map(|i| {
                                    if let Expr::Literal(Value::Str(s)) = i {
                                        Some(s.clone())
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

/// Register a user-declared sub name so it can be recognized as a call without parens.
pub(in crate::parser) fn register_user_sub(name: &str) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current.user_subs.insert(name.to_string());
    });
    // Circumfix/postcircumfix subs change how expressions parse, so invalidate memos.
    if name.starts_with("circumfix:") || name.starts_with("postcircumfix:") {
        crate::parser::invalidate_all_memos();
    }
}

pub(in crate::parser) fn register_user_infix_assoc(name: &str, assoc: &str) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current
            .infix_assoc
            .insert(name.to_string(), assoc.to_string());
    });
}

pub(in crate::parser) fn lookup_user_infix_assoc(symbol: &str) -> Option<String> {
    let key = format!("infix:<{}>", symbol);
    SCOPES.with(|s| {
        let scopes = s.borrow();
        for scope in scopes.iter().rev() {
            if let Some(v) = scope.infix_assoc.get(&key) {
                return Some(v.clone());
            }
        }
        None
    })
}

/// Register a user-declared sub with `is test-assertion`.
pub(in crate::parser) fn register_user_test_assertion_sub(name: &str) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current.test_assertion_subs.insert(name.to_string());
    });
}

/// Reset all scopes (called at parse start).
pub(in crate::parser) fn reset_user_subs() {
    SCOPES.with(|s| {
        *s.borrow_mut() = vec![LexicalScope::default()];
    });
    // Apply pre-seeded operator names (for EVAL context)
    EVAL_OPERATOR_PRESEED.with(|preseed| {
        let names = preseed.borrow();
        for name in names.iter() {
            register_user_sub(name);
            register_user_callable_term_symbol(name);
        }
    });
    EVAL_OPERATOR_ASSOC_PRESEED.with(|preseed| {
        let assoc_map = preseed.borrow();
        for (name, assoc) in assoc_map.iter() {
            register_user_infix_assoc(name, assoc);
        }
    });
}

/// Set operator sub names to pre-register after scope reset (for EVAL).
pub(in crate::parser) fn set_eval_operator_preseed(names: Vec<String>) {
    EVAL_OPERATOR_PRESEED.with(|preseed| {
        *preseed.borrow_mut() = names;
    });
}

pub(in crate::parser) fn set_eval_operator_assoc_preseed(assoc: HashMap<String, String>) {
    EVAL_OPERATOR_ASSOC_PRESEED.with(|preseed| {
        *preseed.borrow_mut() = assoc;
    });
}

/// Check if a name was declared as a user sub in any enclosing scope.
pub(in crate::parser) fn is_user_declared_sub(name: &str) -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        scopes
            .iter()
            .rev()
            .any(|scope| scope.user_subs.contains(name))
    })
}

/// Match a user-declared prefix operator against the current input.
/// Returns `(full_name, consumed_len)` when input begins with an in-scope
/// `prefix:<...>` operator symbol.
pub(in crate::parser) fn match_user_declared_prefix_op(input: &str) -> Option<(String, usize)> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let mut best: Option<(String, usize)> = None;

        for scope in scopes.iter().rev() {
            for name in &scope.user_subs {
                let Some(op) = name
                    .strip_prefix("prefix:<")
                    .and_then(|s| s.strip_suffix('>'))
                else {
                    continue;
                };
                if !input.starts_with(op) {
                    continue;
                }
                let consumed = op.len();
                // For word-like operators, require identifier boundary.
                if op
                    .as_bytes()
                    .last()
                    .copied()
                    .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                    && input
                        .as_bytes()
                        .get(consumed)
                        .copied()
                        .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                {
                    continue;
                }
                if best
                    .as_ref()
                    .is_none_or(|(_, best_len)| consumed > *best_len)
                {
                    best = Some((name.clone(), consumed));
                }
            }
        }
        best
    })
}

/// Match a user-declared postfix operator against the current input.
/// Returns `(full_name, consumed_len)` when input begins with an in-scope
/// `postfix:<...>` operator symbol.
pub(in crate::parser) fn match_user_declared_postfix_op(input: &str) -> Option<(String, usize)> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let mut best: Option<(String, usize)> = None;

        for scope in scopes.iter().rev() {
            for name in &scope.user_subs {
                let Some(op) = name
                    .strip_prefix("postfix:<")
                    .and_then(|s| s.strip_suffix('>'))
                else {
                    continue;
                };
                if !input.starts_with(op) {
                    continue;
                }
                let consumed = op.len();
                // For word-like operators, require identifier boundary.
                if op
                    .as_bytes()
                    .last()
                    .copied()
                    .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                    && input
                        .as_bytes()
                        .get(consumed)
                        .copied()
                        .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                {
                    continue;
                }
                if best
                    .as_ref()
                    .is_none_or(|(_, best_len)| consumed > *best_len)
                {
                    best = Some((name.clone(), consumed));
                }
            }
        }
        best
    })
}

/// Register a user-declared term symbol.
/// The canonical name must be in `term:<...>` form.
pub(in crate::parser) fn register_user_term_symbol(name: &str) {
    let Some(symbol) = name
        .strip_prefix("term:<")
        .and_then(|s| s.strip_suffix('>'))
    else {
        return;
    };
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current
            .term_symbols
            .insert(symbol.to_string(), TermBinding::Value(name.to_string()));
    });
}

pub(in crate::parser) fn register_user_callable_term_symbol(name: &str) {
    let Some(symbol) = name
        .strip_prefix("term:<")
        .and_then(|s| s.strip_suffix('>'))
    else {
        return;
    };
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current
            .term_symbols
            .insert(symbol.to_string(), TermBinding::Callable(name.to_string()));
    });
}

/// Resolve an in-scope term symbol from the current input.
/// Returns `(canonical_name, consumed_len)` when the input begins with a declared symbol.
pub(in crate::parser) fn match_user_declared_term_symbol(
    input: &str,
) -> Option<(String, usize, bool)> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let mut best: Option<(String, usize, bool)> = None;

        for scope in scopes.iter().rev() {
            for (symbol, binding) in &scope.term_symbols {
                if !input.starts_with(symbol) {
                    continue;
                }
                let consumed = symbol.len();
                // For word-like symbols, require identifier boundary.
                if symbol
                    .as_bytes()
                    .last()
                    .copied()
                    .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                    && input
                        .as_bytes()
                        .get(consumed)
                        .copied()
                        .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                {
                    continue;
                }
                if best
                    .as_ref()
                    .is_none_or(|(_, best_len, _)| consumed > *best_len)
                {
                    match binding {
                        TermBinding::Value(canonical) => {
                            best = Some((canonical.clone(), consumed, false));
                        }
                        TermBinding::Callable(canonical) => {
                            best = Some((canonical.clone(), consumed, true));
                        }
                    }
                }
            }
        }
        best
    })
}

/// Match a user-declared circumfix operator against the current input.
/// Returns `(full_name, open_len, close_delim)` when input begins with an in-scope
/// `circumfix:<open close>` operator's opening delimiter.
pub(in crate::parser) fn match_user_declared_circumfix_op(
    input: &str,
) -> Option<(String, usize, String)> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let mut best: Option<(String, usize, String)> = None;

        for scope in scopes.iter().rev() {
            for name in &scope.user_subs {
                let Some(delims) = name
                    .strip_prefix("circumfix:<")
                    .and_then(|s| s.strip_suffix('>'))
                else {
                    continue;
                };
                // Split into open and close delimiters (space-separated)
                let parts: Vec<&str> = delims.split_whitespace().collect();
                if parts.len() != 2 {
                    continue;
                }
                let open = parts[0];
                let close = parts[1];
                if !input.starts_with(open) {
                    continue;
                }
                let consumed = open.len();
                if best
                    .as_ref()
                    .is_none_or(|(_, best_len, _)| consumed > *best_len)
                {
                    best = Some((name.clone(), consumed, close.to_string()));
                }
            }
        }
        best
    })
}

/// Match a user-declared postcircumfix operator against the current input.
/// Returns `(full_name, open_len, close_delim)` when input begins with an in-scope
/// `postcircumfix:<open close>` operator's opening delimiter.
pub(in crate::parser) fn match_user_declared_postcircumfix_op(
    input: &str,
) -> Option<(String, usize, String)> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let mut best: Option<(String, usize, String)> = None;

        for scope in scopes.iter().rev() {
            for name in &scope.user_subs {
                let Some(delims) = name
                    .strip_prefix("postcircumfix:<")
                    .and_then(|s| s.strip_suffix('>'))
                else {
                    continue;
                };
                // Split into open and close delimiters (space-separated)
                let parts: Vec<&str> = delims.split_whitespace().collect();
                if parts.len() != 2 {
                    continue;
                }
                let open = parts[0];
                let close = parts[1];
                if !input.starts_with(open) {
                    continue;
                }
                let consumed = open.len();
                if best
                    .as_ref()
                    .is_none_or(|(_, best_len, _)| consumed > *best_len)
                {
                    best = Some((name.clone(), consumed, close.to_string()));
                }
            }
        }
        best
    })
}

/// Check if the input starts with a registered circumfix or postcircumfix closing delimiter.
/// Used to prevent the postfix operator parser from consuming closing delimiters.
pub(in crate::parser) fn is_circumfix_close_delimiter(input: &str) -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        for scope in scopes.iter().rev() {
            for name in &scope.user_subs {
                let delims = if let Some(d) = name
                    .strip_prefix("circumfix:<")
                    .and_then(|s| s.strip_suffix('>'))
                {
                    d
                } else if let Some(d) = name
                    .strip_prefix("postcircumfix:<")
                    .and_then(|s| s.strip_suffix('>'))
                {
                    d
                } else {
                    continue;
                };
                let parts: Vec<&str> = delims.split_whitespace().collect();
                if parts.len() == 2 && input.starts_with(parts[1]) {
                    return true;
                }
            }
        }
        false
    })
}

/// Register a compile-time constant value (for resolving `<<$x>>` in operator names).
pub(in crate::parser) fn register_compile_time_constant(name: &str, value: String) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current
            .compile_time_constants
            .insert(name.to_string(), value);
    });
}

/// Look up a compile-time constant by name.
pub(in crate::parser) fn lookup_compile_time_constant(name: &str) -> Option<String> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        for scope in scopes.iter().rev() {
            if let Some(value) = scope.compile_time_constants.get(name) {
                return Some(value.clone());
            }
        }
        None
    })
}

/// Check if a name was declared as a test assertion sub in any enclosing scope.
pub(in crate::parser) fn is_user_test_assertion_sub(name: &str) -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        scopes
            .iter()
            .rev()
            .any(|scope| scope.test_assertion_subs.contains(name))
    })
}

/// Check if the callable should carry test assertion caller-site metadata.
pub(in crate::parser) fn is_test_assertion_callable(name: &str) -> bool {
    TEST_ASSERTION_EXPORTS.contains(&name) || is_user_test_assertion_sub(name)
}

/// Push a new lexical scope (called when entering a `{ }` block).
pub(in crate::parser) fn push_scope() {
    SCOPES.with(|s| {
        s.borrow_mut().push(LexicalScope::default());
    });
}

/// Pop the current lexical scope (called when leaving a `{ }` block).
pub(in crate::parser) fn pop_scope() {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        if scopes.len() > 1 {
            scopes.pop();
        }
    });
}

/// Check if a function name was registered via `use` module import.
/// Searches all scopes from innermost to outermost.
pub(crate) fn is_imported_function(name: &str) -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        scopes
            .iter()
            .rev()
            .any(|scope| scope.imported_functions.contains(name))
    })
}

/// Register exported function names for a module (called when parsing `use` statements).
/// Exports are added to the current (innermost) lexical scope.
///
/// For `Test`, uses a hardcoded list (Test functions are implemented natively in Rust).
/// For all other modules, dynamically scans the module file to extract `is export` subs.
pub(in crate::parser) fn register_module_exports(module: &str) {
    let exports: Vec<String> = if module == "Test" {
        TEST_EXPORTS.iter().map(|s| (*s).to_string()).collect()
    } else {
        // Check for infinite recursion
        let already_loading = LOADING_MODULES.with(|m| m.borrow().contains(module));
        if already_loading {
            return;
        }
        LOADING_MODULES.with(|m| {
            m.borrow_mut().insert(module.to_string());
        });
        let result = find_and_extract_exports(module);
        LOADING_MODULES.with(|m| {
            m.borrow_mut().remove(module);
        });
        result
    };
    if exports.is_empty() {
        return;
    }
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        for name in &exports {
            current.imported_functions.insert(name.clone());
        }
    });
}

/// Find a module file and extract its exported function names.
fn find_and_extract_exports(module: &str) -> Vec<String> {
    let path = find_module_file(module);
    match path {
        Some(p) => {
            if let Ok(source) = std::fs::read_to_string(&p) {
                extract_exported_names(&source)
            } else {
                Vec::new()
            }
        }
        None => Vec::new(),
    }
}

/// Search lib_paths and program directory for a `.rakumod` file matching the module name.
fn find_module_file(module: &str) -> Option<String> {
    let filename = format!("{}.rakumod", module.replace("::", "/"));
    // First, search configured lib paths
    let result = LIB_PATHS.with(|paths| {
        let paths = paths.borrow();
        for base in paths.iter() {
            let base_path = std::path::Path::new(base);
            let candidate = base_path.join(&filename);
            if candidate.exists() {
                return Some(candidate.to_string_lossy().into_owned());
            }
            // Also check lib/ subdirectory
            let candidate = base_path.join("lib").join(&filename);
            if candidate.exists() {
                return Some(candidate.to_string_lossy().into_owned());
            }
        }
        None
    });
    if result.is_some() {
        return result;
    }
    // Fall back: search relative to program file (same as runtime's load_module)
    PROGRAM_PATH.with(|pp| {
        let pp = pp.borrow();
        if let Some(path) = pp.as_ref()
            && let Some(parent) = std::path::Path::new(path).parent()
        {
            let candidate = parent.join(&filename);
            if candidate.exists() {
                return Some(candidate.to_string_lossy().into_owned());
            }
        }
        // Last resort: current directory
        let candidate = std::path::Path::new(".").join(&filename);
        if candidate.exists() {
            return Some(candidate.to_string_lossy().into_owned());
        }
        None
    })
}

/// Parse module source and extract names of `is export` sub/proto declarations.
/// Saves and restores the parser's scope state to avoid clobbering the caller's scopes.
fn extract_exported_names(source: &str) -> Vec<String> {
    // Save current scopes — parse_program_partial calls reset_user_subs which clears them
    let saved_scopes = SCOPES.with(|s| s.borrow().clone());
    let (stmts, _) = crate::parser::parse_program_partial(source);
    // Restore scopes
    SCOPES.with(|s| {
        *s.borrow_mut() = saved_scopes;
    });
    let mut names = HashSet::new();
    for stmt in &stmts {
        match stmt {
            Stmt::SubDecl {
                name, is_export, ..
            } if *is_export => {
                names.insert(name.clone());
            }
            Stmt::ProtoDecl {
                name, is_export, ..
            } if *is_export => {
                names.insert(name.clone());
            }
            _ => {}
        }
    }
    // Fallback scan for modules that use syntax not yet fully covered by parse_program_partial.
    // This keeps imported exported-callables discoverable for statement-call parsing.
    for name in extract_exported_names_fallback(source) {
        names.insert(name);
    }

    let mut names: Vec<String> = names.into_iter().collect();
    names.sort();
    names
}

fn extract_exported_names_fallback(source: &str) -> Vec<String> {
    // `sub foo(...) is export`
    // `multi sub foo(...) is export`
    // `proto sub foo(|) is export`
    let sub_re = Regex::new(
        r"\b(?:our\s+)?(?:proto\s+|multi\s+)?sub\s+([A-Za-z_][A-Za-z0-9_'\-]*)\b[^;{]*\bis\s+export\b",
    )
    .expect("valid exported-sub regex");
    // `proto foo(|) is export` (without the `sub` keyword)
    let proto_re = Regex::new(r"\bproto\s+([A-Za-z_][A-Za-z0-9_'\-]*)\b[^;{]*\bis\s+export\b")
        .expect("valid exported-proto regex");

    let mut names = HashSet::new();
    for caps in sub_re.captures_iter(source) {
        if let Some(name) = caps.get(1) {
            names.insert(name.as_str().to_string());
        }
    }
    for caps in proto_re.captures_iter(source) {
        if let Some(name) = caps.get(1) {
            names.insert(name.as_str().to_string());
        }
    }

    let mut names: Vec<String> = names.into_iter().collect();
    names.sort();
    names
}

/// Functions exported by `use Test`.
/// Test functions are implemented natively in Rust (`test_functions.rs`),
/// not loaded from a `.rakumod` file, so they must be hardcoded here.
const TEST_EXPORTS: &[&str] = &[
    "ok",
    "nok",
    "is",
    "isnt",
    "is-deeply",
    "is-approx",
    "cmp-ok",
    "like",
    "unlike",
    "isa-ok",
    "does-ok",
    "can-ok",
    "lives-ok",
    "dies-ok",
    "eval-lives-ok",
    "eval-dies-ok",
    "throws-like",
    "fails-like",
    "pass",
    "flunk",
    "skip",
    "skip-rest",
    "todo",
    "diag",
    "plan",
    "done-testing",
    "bail-out",
    "subtest",
    "use-ok",
    "force_todo",
    "force-todo",
    "tap-ok",
];

const TEST_ASSERTION_EXPORTS: &[&str] = &[
    "ok",
    "nok",
    "is",
    "isnt",
    "is-deeply",
    "is-approx",
    "cmp-ok",
    "isa-ok",
    "does-ok",
    "can-ok",
    "lives-ok",
    "dies-ok",
    "eval-lives-ok",
    "eval-dies-ok",
    "throws-like",
    "fails-like",
    "pass",
    "flunk",
    "use-ok",
    "tap-ok",
];

use super::{
    block, ident, is_stmt_modifier_keyword, keyword, parse_comma_or_expr, parse_statement_modifier,
    parse_stmt_call_args, parse_stmt_call_args_no_paren, statement,
};

/// Check if `say`/`print`/`put` is used bare (no arguments) — this is a compile error in Raku.
fn check_bare_io_func<'a>(name: &str, rest: &'a str) -> PResult<'a, ()> {
    let trimmed = rest.trim_start();
    if trimmed.is_empty() || trimmed.starts_with(';') || trimmed.starts_with('}') {
        return Err(PError::fatal(format!(
            "X::Comp: Unsupported use of bare \"{}\". \
             In Raku please use: .{} if you meant to call it as a method on $_, \
             or use an explicit invocant or argument, \
             or use &{} to refer to the function as a noun.",
            name, name, name
        )));
    }
    Ok((rest, ()))
}

/// Check if `say`/`print`/`put` is followed by `for`/`while`/`until` — X::Obsolete error.
fn check_io_func_followed_by_loop<'a>(name: &str, rest_after_ws: &'a str) -> PResult<'a, ()> {
    for kw in &["for", "while", "until"] {
        if let Some(r) = keyword(kw, rest_after_ws) {
            let next_char = r.chars().next();
            if next_char.is_none()
                || next_char == Some(' ')
                || next_char == Some('\t')
                || next_char == Some('\n')
            {
                return Err(PError::fatal(format!(
                    "X::Obsolete: Unsupported use of bare \"{}\". \
                     In Raku please use: .{} if you meant to call it as a method on $_, \
                     or use an explicit invocant or argument, \
                     or use &{} to refer to the function as a noun.",
                    name, name, name
                )));
            }
        }
    }
    Ok((rest_after_ws, ()))
}

#[cfg(test)]
mod tests {
    use super::extract_exported_names;

    #[test]
    fn extract_exported_names_fallback_handles_proto_and_kebab_case_names() {
        let source = r#"
proto sub is_run(|) is export {*}
sub get_out(Str $code, :@compiler-args) is export { }
proto doesn't-hang(|) is export {*}
sub helper() { }
"#;
        let names = extract_exported_names(source);
        assert!(names.contains(&"is_run".to_string()));
        assert!(names.contains(&"get_out".to_string()));
        assert!(names.contains(&"doesn't-hang".to_string()));
        assert!(!names.contains(&"helper".to_string()));
    }
}

/// Parse a `say` statement.
pub(super) fn say_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("say", input).ok_or_else(|| PError::expected("say statement"))?;
    check_bare_io_func("say", rest)?;
    let (rest, _) = ws1(rest)?;
    check_io_func_followed_by_loop("say", rest)?;
    if let Ok((rest, stmt)) = parse_io_colon_invocant_stmt(rest, "say") {
        return parse_statement_modifier(rest, stmt);
    }
    let (rest, args) = parse_expr_list(rest)?;
    let stmt = Stmt::Say(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `print` statement.
pub(super) fn print_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("print", input).ok_or_else(|| PError::expected("print statement"))?;
    check_bare_io_func("print", rest)?;
    let (rest, _) = ws1(rest)?;
    check_io_func_followed_by_loop("print", rest)?;
    if let Ok((rest, stmt)) = parse_io_colon_invocant_stmt(rest, "print") {
        return parse_statement_modifier(rest, stmt);
    }
    let (rest, args) = parse_expr_list(rest)?;
    let stmt = Stmt::Print(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `put` statement.
pub(super) fn put_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("put", input).ok_or_else(|| PError::expected("put statement"))?;
    check_bare_io_func("put", rest)?;
    let (rest, _) = ws1(rest)?;
    check_io_func_followed_by_loop("put", rest)?;
    if let Ok((rest, stmt)) = parse_io_colon_invocant_stmt(rest, "put") {
        return parse_statement_modifier(rest, stmt);
    }
    let (rest, args) = parse_expr_list(rest)?;
    let stmt = Stmt::Say(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `note` statement.
pub(super) fn note_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("note", input).ok_or_else(|| PError::expected("note statement"))?;
    // `note` with no arguments is valid (prints "Noted\n")
    if let Ok((rest2, _)) = ws1(rest)
        && let Ok((rest3, args)) = parse_expr_list(rest2)
    {
        return parse_statement_modifier(rest3, Stmt::Note(args));
    }
    // Bare `note` with no args
    parse_statement_modifier(rest, Stmt::Note(vec![]))
}

/// Parse a comma-separated expression list.
pub(super) fn parse_expr_list(input: &str) -> PResult<'_, Vec<Expr>> {
    let (input, first) = expression(input)?;
    let mut items = vec![first];
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        if !r.starts_with(',') {
            return Ok((r, items));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        // Check for end of list
        if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
            return Ok((r, items));
        }
        // Check for statement modifier keywords
        if is_stmt_modifier_keyword(r) {
            return Ok((r, items));
        }
        let (r, next) = expression(r)?;
        items.push(next);
        rest = r;
    }
}

fn parse_io_colon_invocant_stmt<'a>(input: &'a str, method_name: &str) -> PResult<'a, Stmt> {
    let (rest_after_target, target) = expression(input)?;
    let (rest_after_target, _) = ws(rest_after_target)?;
    if !rest_after_target.starts_with(':') || rest_after_target.starts_with("::") {
        return Err(PError::expected("io colon invocant call"));
    }
    let mut rest = &rest_after_target[1..];
    let (r, _) = ws(rest)?;
    rest = r;
    let (mut rest_after_args, first_arg) = expression(rest).map_err(|err| PError {
        messages: merge_expected_messages(
            "expected expression after ':' in io invocant call",
            &err.messages,
        ),
        remaining_len: err.remaining_len.or(Some(rest.len())),
    })?;
    let mut args = vec![first_arg];
    loop {
        let (r, _) = ws(rest_after_args)?;
        if !r.starts_with(',') {
            break;
        }
        let r = &r[1..];
        let (r, _) = ws(r)?;
        if r.starts_with(';')
            || r.is_empty()
            || r.starts_with('}')
            || r.starts_with(')')
            || is_stmt_modifier_keyword(r)
        {
            break;
        }
        let (r, next) = expression(r)?;
        args.push(next);
        rest_after_args = r;
    }
    Ok((
        rest_after_args,
        Stmt::Expr(Expr::MethodCall {
            target: Box::new(target),
            name: method_name.to_string(),
            args,
            modifier: None,
            quoted: false,
        }),
    ))
}

/// Parse `return` statement.
pub(super) fn return_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("return", input).ok_or_else(|| PError::expected("return statement"))?;
    let (rest, _) = ws(rest)?;
    if is_stmt_modifier_keyword(rest) {
        return parse_statement_modifier(rest, Stmt::Return(Expr::Literal(Value::Nil)));
    }
    if rest.starts_with(';') || rest.is_empty() || rest.starts_with('}') {
        let (rest, _) = opt_char(rest, ';');
        return Ok((rest, Stmt::Return(Expr::Literal(Value::Nil))));
    }
    let (rest, expr) = parse_comma_or_expr(rest).map_err(|err| PError {
        messages: merge_expected_messages("expected return value expression", &err.messages),
        remaining_len: err.remaining_len.or(Some(rest.len())),
    })?;
    let stmt = Stmt::Return(expr);
    parse_statement_modifier(rest, stmt)
}

/// Parse `last` / `next` / `redo`.
pub(super) fn last_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("last", input).ok_or_else(|| PError::expected("last statement"))?;
    let (rest, _) = ws(rest)?;
    // Check for label: last LABEL
    if rest.starts_with(|c: char| c.is_ascii_uppercase())
        && let Ok((r, label)) = ident(rest)
        && label.chars().all(|c| c.is_ascii_uppercase() || c == '_')
    {
        return parse_statement_modifier(r, Stmt::Last(Some(label)));
    }
    parse_statement_modifier(rest, Stmt::Last(None))
}

pub(super) fn next_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("next", input).ok_or_else(|| PError::expected("next statement"))?;
    let (rest, _) = ws(rest)?;
    // Check for label: next LABEL
    if rest.starts_with(|c: char| c.is_ascii_uppercase())
        && let Ok((r, label)) = ident(rest)
        && label.chars().all(|c| c.is_ascii_uppercase() || c == '_')
    {
        return parse_statement_modifier(r, Stmt::Next(Some(label)));
    }
    parse_statement_modifier(rest, Stmt::Next(None))
}

pub(super) fn redo_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("redo", input).ok_or_else(|| PError::expected("redo statement"))?;
    let (rest, _) = ws(rest)?;
    // Check for label: redo LABEL
    if rest.starts_with(|c: char| c.is_ascii_uppercase())
        && let Ok((r, label)) = ident(rest)
        && label.chars().all(|c| c.is_ascii_uppercase() || c == '_')
    {
        return parse_statement_modifier(r, Stmt::Redo(Some(label)));
    }
    parse_statement_modifier(rest, Stmt::Redo(None))
}

/// Parse `goto` statement.
pub(super) fn goto_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("goto", input).ok_or_else(|| PError::expected("goto statement"))?;
    let (rest, _) = ws1(rest)?;
    // Bare identifier labels are treated as label names, not variable lookups.
    if let Ok((r, label)) = ident(rest) {
        return parse_statement_modifier(r, Stmt::Goto(Expr::Literal(Value::Str(label))));
    }
    let (rest, expr) = expression(rest)?;
    parse_statement_modifier(rest, Stmt::Goto(expr))
}

/// Parse `die` statement.
pub(super) fn die_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, is_fail) = if let Some(r) = keyword("fail", input) {
        (r, true)
    } else {
        let r = keyword("die", input).ok_or_else(|| PError::expected("die/fail statement"))?;
        (r, false)
    };
    let (rest, _) = ws(rest)?;
    // `die`/`fail` with no argument: followed by `;`, end, `}`, or a statement modifier
    let no_arg = rest.starts_with(';')
        || rest.is_empty()
        || rest.starts_with('}')
        || is_stmt_modifier_keyword(rest);
    if no_arg {
        let (rest, _) = opt_char(rest, ';');
        // `die`/`fail` with no argument should reuse current `$!` when present.
        // A later runtime fallback handles Nil -> default text.
        let stmt = if is_fail {
            Stmt::Fail(Expr::Var("!".to_string()))
        } else {
            Stmt::Die(Expr::Var("!".to_string()))
        };
        return parse_statement_modifier(rest, stmt);
    }
    let (rest, expr) = expression(rest)?;
    let stmt = if is_fail {
        Stmt::Fail(expr)
    } else {
        Stmt::Die(expr)
    };
    parse_statement_modifier(rest, stmt)
}

/// Parse `take` statement.
pub(super) fn take_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("take", input).ok_or_else(|| PError::expected("take statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, expr) = expression(rest)?;
    parse_statement_modifier(rest, Stmt::Take(expr))
}

/// Parse CATCH/CONTROL blocks.
pub(super) fn catch_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("CATCH", input).ok_or_else(|| PError::expected("CATCH block"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Catch(body)))
}

pub(super) fn control_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("CONTROL", input).ok_or_else(|| PError::expected("CONTROL block"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Control(body)))
}

/// Parse phasers: BEGIN, END, etc.
pub(super) fn phaser_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, kind) = if let Some(r) = keyword("BEGIN", input) {
        (r, PhaserKind::Begin)
    } else if let Some(r) = keyword("CHECK", input) {
        (r, PhaserKind::Check)
    } else if let Some(r) = keyword("INIT", input) {
        (r, PhaserKind::Init)
    } else if let Some(r) = keyword("END", input) {
        (r, PhaserKind::End)
    } else if let Some(r) = keyword("ENTER", input) {
        (r, PhaserKind::Enter)
    } else if let Some(r) = keyword("LEAVE", input) {
        (r, PhaserKind::Leave)
    } else if let Some(r) = keyword("KEEP", input) {
        (r, PhaserKind::Keep)
    } else if let Some(r) = keyword("UNDO", input) {
        (r, PhaserKind::Undo)
    } else if let Some(r) = keyword("FIRST", input) {
        (r, PhaserKind::First)
    } else if let Some(r) = keyword("NEXT", input) {
        (r, PhaserKind::Next)
    } else if let Some(r) = keyword("LAST", input) {
        (r, PhaserKind::Last)
    } else if let Some(r) = keyword("PRE", input) {
        (r, PhaserKind::Pre)
    } else if let Some(r) = keyword("POST", input) {
        (r, PhaserKind::Post)
    } else if let Some(r) = keyword("QUIT", input) {
        (r, PhaserKind::Quit)
    } else if let Some(r) = keyword("CLOSE", input) {
        (r, PhaserKind::Close)
    } else {
        return Err(PError::expected("phaser keyword"));
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = if rest.starts_with('{') {
        block(rest)?
    } else {
        let (r, s) = statement(rest)?;
        (r, vec![s])
    };
    Ok((rest, Stmt::Phaser { kind, body }))
}

/// Parse `subtest` declaration.
pub(super) fn subtest_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("subtest", input).ok_or_else(|| PError::expected("subtest statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    // Expect =>
    if !rest.starts_with("=>") {
        return Err(PError::expected("'=>' in subtest"));
    }
    let rest = &rest[2..];
    let (rest, _) = ws(rest)?;
    // Optional 'sub' keyword
    let rest = if let Some(r) = keyword("sub", rest) {
        let (r, _) = ws(r)?;
        r
    } else {
        rest
    };
    let (rest, body) = block(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Subtest { name, body }))
}

/// Parse a block statement: { ... }
/// If the block is followed by `.method(...)`, treat it as a block expression
/// with postfix operators (e.g. `{ $^a }.assuming(123)()`).
pub(super) fn block_stmt(input: &str) -> PResult<'_, Stmt> {
    // Try to parse as a hash expression first (e.g. `{:a(4)}`, `{a => 1}`)
    if let Ok((rest, hash_expr)) = crate::parser::primary::misc::block_or_hash_expr(input)
        && matches!(hash_expr, Expr::Hash(_))
    {
        return parse_statement_modifier(rest, Stmt::Expr(hash_expr));
    }
    let (rest, body) = block(input)?;
    let (r_ws, _) = ws(rest)?;
    if r_ws.starts_with('.') && !r_ws.starts_with("..") {
        // Use AnonSub so the block compiles as a closure value, not inline code
        let block_expr = Expr::AnonSub { body, is_rw: false };
        let (rest, expr) = super::super::expr::postfix_expr_continue(rest, block_expr)?;
        return parse_statement_modifier(rest, Stmt::Expr(expr));
    }
    parse_statement_modifier(rest, Stmt::Block(body))
}

fn method_lvalue_assign_expr(
    target: Expr,
    target_var_name: Option<String>,
    method_name: String,
    method_args: Vec<Expr>,
    value: Expr,
) -> Expr {
    let mut args = vec![
        target,
        Expr::Literal(Value::Str(method_name)),
        Expr::ArrayLiteral(method_args),
        value,
    ];
    args.push(match target_var_name {
        Some(name) => Expr::Literal(Value::Str(name)),
        None => Expr::Literal(Value::Nil),
    });
    Expr::Call {
        name: "__mutsu_assign_method_lvalue".to_string(),
        args,
    }
}

fn named_sub_lvalue_assign_expr(name: String, call_args: Vec<Expr>, value: Expr) -> Expr {
    Expr::Call {
        name: "__mutsu_assign_named_sub_lvalue".to_string(),
        args: vec![
            Expr::Literal(Value::Str(name)),
            Expr::ArrayLiteral(call_args),
            value,
        ],
    }
}

fn callable_lvalue_assign_expr(target: Expr, call_args: Vec<Expr>, value: Expr) -> Expr {
    Expr::Call {
        name: "__mutsu_assign_callable_lvalue".to_string(),
        args: vec![target, Expr::ArrayLiteral(call_args), value],
    }
}

/// Parse an expression statement (fallback).
pub(super) fn expr_stmt(input: &str) -> PResult<'_, Stmt> {
    // Topic mutating method call: .=method(args)
    if let Some(stripped) = input.strip_prefix(".=") {
        let (rest, _) = ws(stripped)?;
        let (rest, method_name) =
            take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let method_name = method_name.to_string();
        let (rest, args) = if rest.starts_with('(') {
            let (r, _) = parse_char(rest, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = super::super::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else {
            (rest, Vec::new())
        };
        let stmt = Stmt::Expr(Expr::MethodCall {
            target: Box::new(Expr::Var("_".to_string())),
            name: method_name,
            args,
            modifier: None,
            quoted: false,
        });
        return parse_statement_modifier(rest, stmt);
    }

    let (rest, expr) = expression(input).map_err(|err| PError {
        messages: merge_expected_messages("expected expression statement", &err.messages),
        remaining_len: err.remaining_len.or(Some(input.len())),
    })?;

    // Check for index assignment after expression
    let (rest, _) = ws(rest)?;
    if let Some(stripped) = rest.strip_prefix(".=") {
        let (stripped, _) = ws(stripped)?;
        let (r, method_name) = take_while1(stripped, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })
        .map_err(|err| PError {
            messages: merge_expected_messages("expected method name after '.='", &err.messages),
            remaining_len: err.remaining_len.or(Some(stripped.len())),
        })?;
        let method_name = method_name.to_string();
        let (r, method_args) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = super::super::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else {
            (r, Vec::new())
        };
        let make_rhs = |target: Expr| Expr::MethodCall {
            target: Box::new(target),
            name: method_name.clone(),
            args: method_args.clone(),
            modifier: None,
            quoted: false,
        };
        match expr {
            Expr::Var(name) => {
                let stmt = Stmt::Assign {
                    name: name.clone(),
                    expr: make_rhs(Expr::Var(name.clone())),
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(r, stmt);
            }
            Expr::ArrayVar(name) => {
                let stmt = Stmt::Assign {
                    name: format!("@{}", name),
                    expr: make_rhs(Expr::ArrayVar(name)),
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(r, stmt);
            }
            Expr::HashVar(name) => {
                let stmt = Stmt::Assign {
                    name: format!("%{}", name),
                    expr: make_rhs(Expr::HashVar(name)),
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(r, stmt);
            }
            Expr::Index { target, index } => {
                let tmp_idx = format!(
                    "__mutsu_idx_{}",
                    TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
                );
                let tmp_idx_expr = Expr::Var(tmp_idx.clone());
                let lhs_expr = Expr::Index {
                    target: target.clone(),
                    index: Box::new(tmp_idx_expr.clone()),
                };
                let assigned_value = make_rhs(lhs_expr);
                let stmt = Stmt::Expr(Expr::DoBlock {
                    body: vec![
                        Stmt::VarDecl {
                            name: tmp_idx.clone(),
                            expr: *index,
                            type_constraint: None,
                            is_state: false,
                            is_our: false,
                            is_dynamic: false,
                            is_export: false,
                            export_tags: Vec::new(),
                        },
                        Stmt::Expr(Expr::IndexAssign {
                            target,
                            index: Box::new(tmp_idx_expr),
                            value: Box::new(assigned_value),
                        }),
                    ],
                    label: None,
                });
                return parse_statement_modifier(r, stmt);
            }
            _ => {
                return Err(PError::expected("assignable expression before '.='"));
            }
        }
    }
    if matches!(expr, Expr::Index { .. }) && rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, value) = parse_comma_or_expr(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected assigned expression after index assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?;
        if let Expr::Index { target, index } = expr {
            let stmt = Stmt::Expr(Expr::IndexAssign {
                target,
                index,
                value: Box::new(value),
            });
            return parse_statement_modifier(rest, stmt);
        }
    }
    if let Expr::Index { target, index } = expr.clone()
        && let Some(rest) = parse_hyper_assign_op(rest)
    {
        let (rest, _) = ws(rest)?;
        let (rest, value) = parse_comma_or_expr(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected assigned expression after hyper assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?;
        let stmt = Stmt::Expr(Expr::IndexAssign {
            target,
            index,
            value: Box::new(value),
        });
        return parse_statement_modifier(rest, stmt);
    }
    if matches!(&expr, Expr::Index { target, .. } if matches!(target.as_ref(), Expr::ArrayVar(_)))
        && rest.starts_with(":=")
    {
        let rest = &rest[2..];
        let (rest, _) = ws(rest)?;
        let (rest, value) = parse_comma_or_expr(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected assigned expression after index bind",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?;
        if let Expr::Index { target, index } = expr {
            let stmt = Stmt::Expr(Expr::IndexAssign {
                target,
                index,
                value: Box::new(Expr::Call {
                    name: "__mutsu_bind_index_value".to_string(),
                    args: vec![value],
                }),
            });
            return parse_statement_modifier(rest, stmt);
        }
    }

    // Generic assignment on non-variable lhs (e.g. `.key = 1`).
    // TODO: Introduce a dedicated assignment AST form for arbitrary lvalues.
    // Current fallback consumes `lhs = rhs` and preserves both sides as expressions.
    if !matches!(expr, Expr::AssignExpr { .. })
        && rest.starts_with('=')
        && !rest.starts_with("==")
        && !rest.starts_with("=>")
    {
        // Assignment to certain method call results is read-only (X::Assignment::RO)
        // Meta-object protocol methods like .HOW, .WHAT, etc. are always immutable
        if let Expr::MethodCall { name: meth, .. } = &expr
            && matches!(
                meth.as_str(),
                "HOW" | "WHAT" | "WHO" | "WHY" | "WHICH" | "WHERE" | "DEFINITE"
            )
        {
            return Err(PError::fatal(format!(
                "X::Assignment::RO: Cannot modify an immutable value via .{}",
                meth
            )));
        }
        let r = &rest[1..];
        let (r, _) = ws(r)?;
        let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after '='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
        })?;
        if let Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } = &expr
        {
            let assigned = if name == "AT-POS" && args.len() == 1 {
                Expr::IndexAssign {
                    target: target.clone(),
                    index: Box::new(args[0].clone()),
                    value: Box::new(rhs),
                }
            } else {
                let target_var_name = match target.as_ref() {
                    Expr::Var(var_name) => Some(var_name.clone()),
                    Expr::ArrayVar(var_name) => Some(format!("@{}", var_name)),
                    Expr::HashVar(var_name) => Some(format!("%{}", var_name)),
                    _ => None,
                };
                method_lvalue_assign_expr(
                    (**target).clone(),
                    target_var_name,
                    name.clone(),
                    args.clone(),
                    rhs,
                )
            };
            let stmt = Stmt::Expr(assigned);
            return parse_statement_modifier(r, stmt);
        }
        if let Expr::Call { name, args } = &expr {
            let stmt = Stmt::Expr(named_sub_lvalue_assign_expr(
                name.clone(),
                args.clone(),
                rhs,
            ));
            return parse_statement_modifier(r, stmt);
        }
        if let Expr::CallOn { target, args } = &expr {
            let stmt = Stmt::Expr(callable_lvalue_assign_expr(
                (**target).clone(),
                args.clone(),
                rhs,
            ));
            return parse_statement_modifier(r, stmt);
        }
        let stmt = Stmt::Block(vec![Stmt::Expr(expr), Stmt::Expr(rhs)]);
        return parse_statement_modifier(r, stmt);
    }

    // Generic bind assignment on non-variable lhs (e.g. `($a, $b) := |(f)`).
    // Keep this as a parse fallback so complex bind lvalues don't fail early.
    if !matches!(expr, Expr::AssignExpr { .. }) && rest.starts_with(":=") {
        let r = &rest[2..];
        let (r, _) = ws(r)?;
        let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after ':='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
        })?;
        let stmt = Stmt::Block(vec![Stmt::Expr(expr), Stmt::Expr(rhs)]);
        return parse_statement_modifier(r, stmt);
    }

    // Check for assignment after parenthesized assign expression: ($x = $y) = 5
    if let Expr::AssignExpr { ref name, .. } = expr {
        if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
            let var_name = name.clone();
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected right-hand expression after '='",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
            })?;
            let stmts = vec![
                Stmt::Expr(expr),
                Stmt::Assign {
                    name: var_name,
                    expr: rhs,
                    op: AssignOp::Assign,
                },
            ];
            return parse_statement_modifier(r, Stmt::Block(stmts));
        }
        if let Some((stripped, op)) = super::assign::parse_compound_assign_op(rest) {
            let var_name = name.clone();
            let (r, _) = ws(stripped)?;
            let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected right-hand expression after compound assignment",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
            })?;
            let var_expr = Expr::Var(var_name.clone());
            let stmts = vec![
                Stmt::Expr(expr),
                Stmt::Assign {
                    name: var_name,
                    expr: Expr::Binary {
                        left: Box::new(var_expr),
                        op: op.token_kind(),
                        right: Box::new(rhs),
                    },
                    op: AssignOp::Assign,
                },
            ];
            return parse_statement_modifier(r, Stmt::Block(stmts));
        }
    }

    // Generic compound assignment on non-variable lhs (e.g. `.key //= ++$i`).
    // TODO: Support proper compound assignment semantics for arbitrary lvalues.
    // Current fallback desugars to a binary op expression so parsing can proceed.
    if !matches!(expr, Expr::AssignExpr { .. })
        && let Some((stripped, op)) = super::assign::parse_compound_assign_op(rest)
    {
        let (r, _) = ws(stripped)?;
        let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
        })?;
        if let Expr::Index { target, index } = &expr {
            let tmp_idx = format!(
                "__mutsu_idx_{}",
                TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
            );
            let tmp_idx_expr = Expr::Var(tmp_idx.clone());
            let lhs_expr = Expr::Index {
                target: target.clone(),
                index: Box::new(tmp_idx_expr.clone()),
            };
            let assigned_value = if matches!(op, super::assign::CompoundAssignOp::DefinedOr) {
                Expr::Ternary {
                    cond: Box::new(Expr::Call {
                        name: "defined".to_string(),
                        args: vec![lhs_expr.clone()],
                    }),
                    then_expr: Box::new(lhs_expr.clone()),
                    else_expr: Box::new(rhs),
                }
            } else {
                Expr::Binary {
                    left: Box::new(lhs_expr.clone()),
                    op: op.token_kind(),
                    right: Box::new(rhs),
                }
            };
            let stmt = Stmt::Expr(Expr::DoBlock {
                body: vec![
                    Stmt::VarDecl {
                        name: tmp_idx.clone(),
                        expr: (*index.clone()),
                        type_constraint: None,
                        is_state: false,
                        is_our: false,
                        is_dynamic: false,
                        is_export: false,
                        export_tags: Vec::new(),
                    },
                    Stmt::Expr(Expr::IndexAssign {
                        target: target.clone(),
                        index: Box::new(tmp_idx_expr),
                        value: Box::new(assigned_value),
                    }),
                ],
                label: None,
            });
            return parse_statement_modifier(r, stmt);
        }
        let stmt = Stmt::Expr(Expr::Binary {
            left: Box::new(expr),
            op: op.token_kind(),
            right: Box::new(rhs),
        });
        return parse_statement_modifier(r, stmt);
    }

    // Check for comma-separated expressions (e.g., "1,2, until $++")
    // The statement modifier applies only to the last expression
    if rest.starts_with(',') && !rest.starts_with(",,") {
        let mut exprs = vec![expr];
        let mut r = rest;

        // Collect comma-separated expressions
        while r.starts_with(',') && !r.starts_with(",,") {
            let r2 = &r[1..];
            let (r2, _) = ws(r2)?;

            // Check if we hit a statement modifier - if so, stop parsing exprs
            if is_stmt_modifier_keyword(r2) {
                r = r2;
                break;
            }

            // Stop at semicolon, closing brace, or end of input
            if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') {
                r = r2;
                break;
            }

            let (r2, next_expr) = expression(r2)?;
            exprs.push(next_expr);
            let (r2, _) = ws(r2)?;
            r = r2;
        }

        // Without a trailing statement modifier, keep this as a plain comma
        // expression statement (do not split into multiple statements).
        if !is_stmt_modifier_keyword(r) {
            let expr = if exprs.len() == 1 {
                exprs.remove(0)
            } else {
                Expr::ArrayLiteral(exprs)
            };
            return Ok((r, Stmt::Expr(expr)));
        }

        // Convert all but last expr to Stmt::Expr for modifier lowering
        let mut stmts = Vec::new();
        let last_expr = exprs.pop().unwrap();
        for e in exprs {
            stmts.push(Stmt::Expr(e));
        }

        // Apply statement modifier to the last expression
        let last_stmt = Stmt::Expr(last_expr);
        let (r, last_stmt_with_modifier) = parse_statement_modifier(r, last_stmt)?;
        stmts.push(last_stmt_with_modifier);

        // Return as a block
        return Ok((r, Stmt::Block(stmts)));
    }

    let stmt = Stmt::Expr(expr);
    parse_statement_modifier(rest, stmt)
}

/// Parse `let` statement: `let $var = expr`, `let $var`, `let @arr[idx] = expr`.
pub(super) fn let_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("let", input).ok_or_else(|| PError::expected("let statement"))?;
    let (rest, _) = ws1(rest)?;
    // Parse sigil + var name
    let sigil = rest
        .chars()
        .next()
        .ok_or_else(|| PError::expected("variable after let"))?;
    if sigil != '$' && sigil != '@' && sigil != '%' {
        return Err(PError::expected("variable after let"));
    }
    let rest_after_sigil = &rest[1..];
    let (rest, var_name) = super::ident(rest_after_sigil)?;
    // Env key: scalars strip $, arrays/hashes keep sigil
    let full_name = if sigil == '$' {
        var_name.clone()
    } else {
        format!("{}{}", sigil, var_name)
    };
    let (rest, _) = ws(rest)?;

    // Check for index: @arr[idx]
    if let Some(idx_rest) = rest.strip_prefix('[') {
        let (idx_rest, _) = ws(idx_rest)?;
        let (idx_rest, idx_expr) = expression(idx_rest)?;
        let (idx_rest, _) = ws(idx_rest)?;
        let (idx_rest, _) = parse_char(idx_rest, ']')?;
        let (idx_rest, _) = ws(idx_rest)?;
        if idx_rest.starts_with('=') && !idx_rest.starts_with("==") {
            let val_rest = &idx_rest[1..];
            let (val_rest, _) = ws(val_rest)?;
            let (val_rest, val_expr) = expression(val_rest)?;
            return parse_statement_modifier(
                val_rest,
                Stmt::Let {
                    name: full_name,
                    index: Some(Box::new(idx_expr)),
                    value: Some(Box::new(val_expr)),
                },
            );
        }
        return parse_statement_modifier(
            idx_rest,
            Stmt::Let {
                name: full_name,
                index: Some(Box::new(idx_expr)),
                value: None,
            },
        );
    }

    // Check for assignment: let $var = expr
    if rest.starts_with('=') && !rest.starts_with("==") {
        let val_rest = &rest[1..];
        let (val_rest, _) = ws(val_rest)?;
        let (val_rest, val_expr) = expression(val_rest)?;
        return parse_statement_modifier(
            val_rest,
            Stmt::Let {
                name: full_name,
                index: None,
                value: Some(Box::new(val_expr)),
            },
        );
    }

    // Bare let: let $var / let @arr / let %hash
    parse_statement_modifier(
        rest,
        Stmt::Let {
            name: full_name,
            index: None,
            value: None,
        },
    )
}

/// Parse `temp` statement — same semantics as `let` (save/restore at scope exit).
pub(super) fn temp_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("temp", input).ok_or_else(|| PError::expected("temp statement"))?;
    let (rest, _) = ws1(rest)?;
    // temp on lvalue method call: `temp $obj.method = value`
    if let Ok((expr_rest, expr)) = expression(rest) {
        let (expr_rest_ws, _) = ws(expr_rest)?;
        if expr_rest_ws.starts_with('=')
            && !expr_rest_ws.starts_with("==")
            && let Expr::MethodCall {
                target,
                name,
                args,
                modifier: _,
                quoted: _,
            } = expr
            && let Expr::Var(var_name) = target.as_ref()
        {
            let rhs_rest = &expr_rest_ws[1..];
            let (rhs_rest, _) = ws(rhs_rest)?;
            let (rhs_rest, rhs_expr) = expression(rhs_rest)?;
            return parse_statement_modifier(
                rhs_rest,
                Stmt::TempMethodAssign {
                    var_name: var_name.clone(),
                    method_name: name,
                    method_args: args,
                    value: rhs_expr,
                },
            );
        }
    }
    // Parse sigil + optional twigil + var name
    let sigil = rest
        .chars()
        .next()
        .ok_or_else(|| PError::expected("variable after temp"))?;
    if sigil != '$' && sigil != '@' && sigil != '%' {
        return Err(PError::expected("variable after temp"));
    }
    let after_sigil = &rest[1..];
    // Handle twigils: $*CWD, $?FILE, etc.
    let (after_twigil, twigil) = if after_sigil.starts_with('*')
        || after_sigil.starts_with('?')
        || after_sigil.starts_with('!')
    {
        (&after_sigil[1..], &after_sigil[..1])
    } else {
        (after_sigil, "")
    };
    let (rest, var_name) = super::ident(after_twigil)?;
    // Build full env key including twigil
    let full_name = if sigil == '$' {
        if twigil.is_empty() {
            var_name.clone()
        } else {
            format!("{}{}", twigil, var_name)
        }
    } else {
        format!("{}{}{}", sigil, twigil, var_name)
    };
    let (rest, _) = ws(rest)?;
    // Check for assignment: temp $*CWD = expr
    if rest.starts_with('=') && !rest.starts_with("==") {
        let val_rest = &rest[1..];
        let (val_rest, _) = ws(val_rest)?;
        let (val_rest, val_expr) = expression(val_rest)?;
        return parse_statement_modifier(
            val_rest,
            Stmt::Let {
                name: full_name,
                index: None,
                value: Some(Box::new(val_expr)),
            },
        );
    }
    // Bare temp: temp $var
    parse_statement_modifier(
        rest,
        Stmt::Let {
            name: full_name,
            index: None,
            value: None,
        },
    )
}

/// Known function names that get Stmt::Call treatment at statement level.
/// Test/Test::Util functions are NOT listed here — they are registered dynamically
/// via `register_module_exports()` when `use Test` / `use Test::Util` is parsed.
pub(super) const KNOWN_CALLS: &[&str] = &[
    "dd", "exit", "proceed", "succeed", "push", "pop", "shift", "unshift", "append", "prepend",
    "elems", "chars", "defined", "warn", "EVAL", "EVALFILE",
];

/// Check if a name is a known statement-level function call.
pub(super) fn is_known_call(name: &str) -> bool {
    KNOWN_CALLS.contains(&name) || is_imported_function(name)
}

/// Return true when parsing this known call as a standalone statement would
/// truncate a larger expression (e.g. `defined($x) ?? 1 !! 2`).
fn known_call_is_expression_prefix(input: &str, rest_after_call: &str) -> bool {
    let Ok((rest_after_call, _)) = ws(rest_after_call) else {
        return false;
    };
    if rest_after_call.is_empty()
        || rest_after_call.starts_with(';')
        || rest_after_call.starts_with('}')
    {
        return false;
    }
    if is_stmt_modifier_keyword(rest_after_call) {
        return false;
    }
    if let Ok((expr_rest, _)) = expression(input) {
        return expr_rest.len() < rest_after_call.len();
    }
    false
}

/// Parse a known function call as statement.
pub(super) fn known_call_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, name) = ident(input)?;
    if !is_known_call(&name) {
        return Err(PError::expected("known function call"));
    }
    let had_ws = rest.starts_with(' ') || rest.starts_with('\t') || rest.starts_with('\n');
    let (rest, _) = ws(rest)?;

    // Special handling for proceed/succeed with no args
    if name == "proceed" {
        let (rest, _) = opt_char(rest, ';');
        return Ok((rest, Stmt::Proceed));
    }
    if name == "succeed" {
        let (rest, _) = opt_char(rest, ';');
        return Ok((rest, Stmt::Succeed));
    }

    // In Raku, `foo(args)` (no space) = paren call, but `foo (expr)` (space) = listop call.
    // When there was whitespace before `(`, treat `(` as expression grouping, not call parens.
    let (rest, args) = if had_ws {
        parse_stmt_call_args_no_paren(rest).map_err(|err| PError {
            messages: merge_expected_messages("expected known call arguments", &err.messages),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?
    } else {
        parse_stmt_call_args(rest).map_err(|err| PError {
            messages: merge_expected_messages("expected known call arguments", &err.messages),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?
    };
    let mut args = args;
    if known_call_is_expression_prefix(input, rest) {
        return Err(PError::expected("known function call"));
    }
    if is_test_assertion_callable(&name) {
        args.push(crate::ast::CallArg::Named {
            name: "__mutsu_test_callsite_line".to_string(),
            value: Some(Expr::Literal(Value::Int(
                crate::parser::primary::current_line_number(input),
            ))),
        });
    }
    let stmt = Stmt::Call { name, args };
    parse_statement_modifier(rest, stmt)
}
