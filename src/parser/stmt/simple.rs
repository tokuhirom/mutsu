use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::AtomicUsize;

use regex::Regex;

use super::super::add_parse_warning;
use super::super::expr::expression;
use super::super::helpers::{is_loop_label_name, skip_balanced_parens, ws, ws1};
use super::super::parse_result::{PError, PResult, merge_expected_messages, opt_char, parse_char};

use crate::ast::{Expr, PhaserKind, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

pub(super) use super::simple_expr_stmt::{expr_stmt, let_stmt, temp_stmt};

use super::{
    block, ident, is_stmt_modifier_keyword, keyword, parse_comma_or_expr, parse_statement_modifier,
    parse_stmt_call_args, parse_stmt_call_args_no_paren, statement,
};

// Themed submodules. This file is the facade: it owns the parser-state statics
// and shared types, declares the submodules, and re-exports each moved function
// at its original visibility so external callers (`simple::<name>`) are unchanged.
mod compile_consts;
mod control_stmts;
mod io_stmts;
mod lib_paths;
mod module_exports;
mod pragma_preseed;
mod registry;
mod user_ops;

// `pub` re-exports.
pub use lib_paths::{clear_parser_lib_paths, set_parser_lib_paths, set_parser_program_path};

// `pub(crate)` re-exports.
pub(crate) use compile_consts::is_imported_function;
pub(crate) use registry::{current_language_version, set_current_language_version};

// `pub(super)` re-exports.
pub(super) use control_stmts::{
    block_stmt, catch_stmt, control_stmt, die_stmt, goto_stmt, known_call_stmt, last_stmt,
    next_stmt, phaser_stmt, redo_stmt, return_stmt, subtest_stmt, take_stmt,
};
pub(super) use io_stmts::{note_stmt, print_stmt, put_stmt, say_stmt};

// `pub(in crate::parser)` re-exports.
pub(in crate::parser) use compile_consts::{
    is_test_assertion_callable, lookup_compile_time_constant, pop_scope, push_scope,
    register_compile_time_constant, suppress_worries, worries_suppressed,
};
pub(in crate::parser) use control_stmts::is_known_call;
pub(in crate::parser) use lib_paths::try_add_parse_time_lib_path;
pub(in crate::parser) use module_exports::{
    import_inline_module_exports, register_inline_module_exports, register_module_exports,
};
pub(in crate::parser) use pragma_preseed::{
    current_attributes_pragma, is_user_declared_sub, is_user_declared_type, register_user_type,
    set_attributes_pragma, set_eval_imported_function_preseed, set_eval_operator_assoc_preseed,
    set_eval_operator_preseed, set_eval_user_sub_preseed,
};
pub(in crate::parser) use registry::{
    lookup_custom_infix_precedence, lookup_postfix_precedence, lookup_prefix_precedence,
    lookup_user_infix_assoc, register_op_precedence, register_user_infix_assoc, register_user_sub,
    register_user_test_assertion_sub, reset_user_subs, resolve_op_precedence,
};
pub(in crate::parser) use user_ops::{
    is_circumfix_close_delimiter, is_user_declared_prefix_sub, is_user_defined_infix,
    match_user_declared_circumfix_op, match_user_declared_infix_symbol_op,
    match_user_declared_postcircumfix_op, match_user_declared_postfix_op,
    match_user_declared_prefix_op, match_user_declared_term_symbol,
    register_user_callable_term_symbol, register_user_term_symbol,
};

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
    /// Operator precedence levels. Key is full operator name (e.g. "infix:<add>"),
    /// value is numeric precedence level.
    op_precedence: HashMap<String, i32>,
    /// User-declared class/role/grammar/enum names. Used to disambiguate
    /// identifiers like `S` from the `S///` substitution operator.
    user_types: HashSet<String>,
    /// `no worries` lexical pragma: when true, compiler "Potential difficulties"
    /// warnings (e.g. the empty-`<>` colonpair warning) are suppressed in this
    /// scope and any nested scopes (inherited via `push_scope`).
    worries_suppressed: bool,
}

#[derive(Clone)]
struct InlineModuleExport {
    name: String,
    precedence: Option<i32>,
    associativity: Option<String>,
    /// True when the exported sub is declared `is test-assertion`. The importer
    /// must re-register it so calls to it in the using file get the caller-line
    /// marker attached at parse time (matching Rakudo's caller-line reporting for
    /// assertion helpers like Test::Assuming's `is-primed-sig`).
    is_test_assertion: bool,
}

pub(in crate::parser) type InlineModuleExportSpec =
    (String, Option<(String, String)>, Option<String>);

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
    /// Imported function names to pre-register after scope reset (for EVAL).
    static EVAL_IMPORTED_FUNCTION_PRESEED: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
    /// User-declared sub names to pre-register after scope reset (for EVAL).
    /// These are subs defined in the outer runtime scope that EVAL'd code
    /// should see as declared (so e.g. `first.uc` can parse as
    /// `first().uc` when a user sub `first` shadows the `first` listop).
    static EVAL_USER_SUB_PRESEED: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
    static CURRENT_LANGUAGE_VERSION: RefCell<String> = RefCell::new("6.d".to_string());
    /// `use attributes :D/:U/:_` pragma — tracks the smiley to apply to unsmileyed attribute types.
    /// Empty string means no pragma active.
    static ATTRIBUTES_PRAGMA: RefCell<String> = const { RefCell::new(String::new()) };
    /// Inline module exports: module name → list of exported sub names.
    /// Populated when parsing `module Foo { sub bar() is export { ... } }` blocks.
    /// Used by `import` to register exported operators at parse time.
    static INLINE_MODULE_EXPORTS: RefCell<HashMap<String, Vec<InlineModuleExport>>> =
        RefCell::new(HashMap::new());
}

pub(in crate::parser) static TMP_INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Numeric precedence levels for built-in operator categories.
/// Higher values mean tighter binding.
pub(in crate::parser) const PREC_SEQUENCE: i32 = 10;
pub(in crate::parser) const PREC_STRUCTURAL: i32 = 20;
pub(in crate::parser) const PREC_CONCAT: i32 = 30;
pub(in crate::parser) const PREC_ADDITIVE: i32 = 40;
pub(in crate::parser) const PREC_MULTIPLICATIVE: i32 = 50;
pub(in crate::parser) const PREC_POWER: i32 = 60;
pub(in crate::parser) const PREC_PREFIX: i32 = 70;

fn flatten_xor_chain_terms<'a>(expr: &'a Expr, out: &mut Vec<&'a Expr>) {
    if let Expr::Binary { left, op, right } = expr
        && *op == TokenKind::XorXor
    {
        flatten_xor_chain_terms(left, out);
        flatten_xor_chain_terms(right, out);
        return;
    }
    out.push(expr);
}

pub(super) fn add_xor_sink_warnings(expr: &Expr) {
    let mut terms = Vec::new();
    flatten_xor_chain_terms(expr, &mut terms);
    if terms.len() < 2 {
        return;
    }
    for term in terms.iter().skip(1) {
        if let Expr::Literal(Value::Str(s)) = term {
            add_parse_warning(format!(
                "Useless use of constant string \"{}\" in sink context (line 1)",
                s
            ));
        }
    }
}

pub(super) fn parse_hyper_assign_op(input: &str) -> Option<&str> {
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

#[cfg(test)]
mod tests {
    use super::module_exports::extract_exported_names;
    use super::{say_stmt, statement};
    use crate::ast::{Expr, Stmt};

    #[test]
    fn extract_exported_names_fallback_handles_proto_and_kebab_case_names() {
        let source = r#"
proto sub is_run(|) is export {*}
sub get_out(Str $code, :@compiler-args) is export { }
proto doesn't-hang(|) is export {*}
sub helper() { }
"#;
        let exports = extract_exported_names(source);
        let names: Vec<String> = exports.iter().map(|e| e.name.clone()).collect();
        assert!(names.contains(&"is_run".to_string()));
        assert!(names.contains(&"get_out".to_string()));
        assert!(names.contains(&"doesn't-hang".to_string()));
        assert!(!names.contains(&"helper".to_string()));
    }

    #[test]
    fn say_stmt_rejects_adjacent_statement_keyword_without_separator() {
        let err = say_stmt("say and die 73266").unwrap_err();
        assert!(
            err.messages
                .iter()
                .any(|msg| msg.contains("comma or statement end after argument"))
        );
    }

    #[test]
    fn statement_does_not_fall_back_after_invalid_say_args() {
        assert!(statement("say and die 73266").is_err());
    }

    #[test]
    fn statement_rewrites_scalar_decl_before_cross_metaop() {
        let (_, stmt) = statement("my $a = (1, 3) X (2, 4);").unwrap();
        match stmt {
            Stmt::Expr(Expr::MetaOp { meta, left, .. }) => {
                assert_eq!(meta, "X");
                match left.as_ref() {
                    Expr::DoStmt(inner) => match inner.as_ref() {
                        Stmt::VarDecl { name, expr, .. } => {
                            assert_eq!(name, "a");
                            assert!(matches!(expr, Expr::ArrayLiteral(items) if items.len() == 2));
                        }
                        other => panic!("expected VarDecl in DoStmt, got {other:?}"),
                    },
                    other => panic!("expected DoStmt on left side, got {other:?}"),
                }
            }
            other => panic!("expected rewritten X meta-op statement, got {other:?}"),
        }
    }

    #[test]
    fn statement_keeps_array_meta_assign_as_assignment() {
        let (_, stmt) = statement("@a [X+]= @b;").unwrap();
        match stmt {
            Stmt::Assign { name, expr, .. } => {
                assert_eq!(name, "@a");
                match expr {
                    Expr::MetaOp {
                        meta,
                        op,
                        left,
                        right,
                    } => {
                        assert_eq!(meta, "X");
                        assert_eq!(op, "+");
                        assert!(matches!(left.as_ref(), Expr::ArrayVar(name) if name == "a"));
                        assert!(matches!(right.as_ref(), Expr::ArrayVar(name) if name == "b"));
                    }
                    other => panic!("expected meta-op rhs, got {other:?}"),
                }
            }
            other => panic!("expected array assignment statement, got {other:?}"),
        }
    }
}
