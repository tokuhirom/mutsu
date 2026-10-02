//! Method-signature helpers shared by the runtime registration walkers
//! (`runtime/registration_class_body_method.rs`, `registration_role_method.rs`,
//! `registration_class_augment.rs`) and the main-pass compiler
//! (`compiler/helpers_method_body.rs`, ADR-0019 D3-8a).
//!
//! These functions compute the *effective* parameter list every method body
//! sees at runtime — the implicit `*%_`/`*@_` slurpies Raku adds to a method
//! signature — from pure AST inputs (no `&self`, no registry access). Moving
//! them here (out of `runtime::registration`/`runtime::methods_signature`,
//! where they were previously `Interpreter` associated functions reachable
//! only from `runtime::*`) gives the compiler and the runtime ONE
//! implementation to call instead of two independently-drifting copies —
//! the same pattern D2b established for `CompiledAttrDecl`.

use crate::ast::{Expr, ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

/// Build an `X::Placeholder::Mainline` (`kind == "mainline"`) or
/// `X::Placeholder::Block` exception value for a placeholder variable used
/// where no signature-capable block can capture it. Shared by the compiler
/// (`compiler::expr_closure`, for the mainline/do-block/class-role-body
/// shapes) and this module's own [`direct_positional_placeholder_die_body`]
/// (the direct-in-a-method-body shape).
pub(crate) fn placeholder_scope_error(kind: &str, placeholder: &str) -> Value {
    let (type_name, message) = if kind == "mainline" {
        (
            "X::Placeholder::Mainline",
            format!(
                "Cannot use placeholder parameter {} outside of a sub or block",
                placeholder
            ),
        )
    } else {
        (
            "X::Placeholder::Block",
            format!(
                "Placeholder variable '{}' may not be used here because the \
                 surrounding block does not take a signature.",
                placeholder
            ),
        )
    };
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message));
    attrs.insert(
        "placeholder".to_string(),
        Value::str(placeholder.to_string()),
    );
    Value::make_instance(Symbol::intern(type_name), attrs)
}

/// The synthetic single-statement body ("die with `X::Placeholder::Block`")
/// that replaces a signature-less method's real body when it reads a bare
/// `@_` DIRECTLY (not nested inside a `do {}`, which
/// `Compiler::compile_do_block_expr` already rejects separately, nor `%_`,
/// which a method legitimately gets as an implicit `*%_`). Raku methods
/// never get an implicit `*@_` — referencing `@_` in a method body is a
/// compile-time error there
/// (`raku -e 'class A { method m { @_.raku.say } }'` =>
/// "Placeholder variables (eg. @_) cannot be used in a method. Please
/// specify an explicit signature, like method m (*@_) { ... }"). mutsu
/// raises this when the method actually runs rather than at compile time
/// (matching how the do{}-nested sibling shape already behaves) — installing
/// this AST in place of the real body keeps every other bit of method
/// registration (byte-parity keys, MRO, dispatch table installation)
/// completely unchanged, since the method is still fully registered and
/// dispatchable, it just always dies when called.
pub(crate) fn direct_positional_placeholder_die_body() -> Vec<Stmt> {
    vec![Stmt::Die(Expr::Literal(placeholder_scope_error(
        "block", "@_",
    )))]
}

/// A *named* slurpy is always `%`-sigiled (`*%foo` or `**%foo`). A
/// double-star slurpy on an `@`-sigiled param (`**@values`) is a
/// slip-preserving slurpy POSITIONAL, not a named one, so it must NOT
/// suppress the implicit `*%_` every method otherwise gets — else `%_`
/// stays `Any` and `self.bless(|%_)` splats a stray `Any` positional
/// into `TWEAK`/`BUILD` (raku: `%_` is always an empty Hash here).
fn has_explicit_named_slurpy(param_defs: &[ParamDef]) -> bool {
    param_defs
        .iter()
        .any(|pd| pd.slurpy && pd.name.starts_with('%'))
}

/// The implicit `*%_` slurpy every non-`is hidden` method signature gets
/// unless the declaration already has an explicit named slurpy.
pub(crate) fn implicit_method_named_slurpy_param() -> ParamDef {
    ParamDef {
        type_capture: None,
        name: "%_".to_string(),
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        named_alias: false,
        slurpy: true,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
        block_param: false,
        code: Default::default(),
        trait_args: Vec::new(),
    }
}

/// The full effective parameter list a method body sees: the declared
/// `param_defs` plus the implicit `*%_` slurpy, unless the class is `is
/// hidden` or the declaration already names an explicit named slurpy.
pub(crate) fn effective_method_param_defs(
    param_defs: &[ParamDef],
    class_is_hidden: bool,
) -> Vec<ParamDef> {
    let mut defs = param_defs.to_vec();
    if !class_is_hidden && !has_explicit_named_slurpy(&defs) {
        defs.push(implicit_method_named_slurpy_param());
    }
    defs
}

/// The implicit `*@_` positional slurpy inserted into a signature-less
/// method's effective param defs when it needs the placeholder-die body
/// ([`needs_direct_positional_placeholder_die_from_flag`]) — so the method
/// still accepts any call arity (the arity check happens at bind time,
/// before the body would run) and the die is what the caller actually
/// observes, regardless of how many arguments they happened to pass.
fn implicit_method_positional_slurpy_param() -> ParamDef {
    ParamDef {
        type_capture: None,
        name: "@_".to_string(),
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        named_alias: false,
        slurpy: true,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
        block_param: false,
        code: Default::default(),
        trait_args: Vec::new(),
    }
}

/// Detect whether a signature-less method body reads a bare `@_` directly
/// (not nested inside a `do {}`, which `Compiler::compile_do_block_expr`
/// already rejects separately). Unlike a `sub`, Raku methods never get an
/// implicit `*@_` — only `*%_` — so a caller that gets `true` back must
/// replace whatever it was going to compile/register with
/// [`direct_positional_placeholder_die_body`] instead of the real body. As a
/// side effect, inserts an implicit `*@_` into `effective_param_defs` (if
/// not already present) so a call with any arity still binds and reaches
/// the die, rather than surfacing a less informative arity-mismatch error
/// first — see `todo/tickets/method-direct-at-underscore-should-be-rejected.md`
/// (now `news/2026-08/`).
///
/// Only applies when the ORIGINAL declaration (`original_param_defs_is_empty`)
/// had no signature at all — an explicit (even empty, `()`) signature opts
/// out, matching `class_body_method_decl`'s `decl.param_defs.is_empty()`
/// guard.
pub(crate) fn needs_direct_positional_placeholder_die(
    original_param_defs_is_empty: bool,
    body: &[Stmt],
    effective_param_defs: &mut Vec<ParamDef>,
) -> bool {
    let (use_positional, _) = auto_signature_uses(body);
    needs_direct_positional_placeholder_die_from_flag(
        original_param_defs_is_empty,
        use_positional,
        effective_param_defs,
    )
}

/// Same check as [`needs_direct_positional_placeholder_die`], but taking the
/// body scan's result directly instead of re-deriving it from `body` — for
/// callers holding a [`crate::opcode::CompiledMethodDecl`], whose
/// `uses_bare_positional_args` field (ADR-0019 D3-9) is precomputed once at
/// plan-lowering/declaration-build time rather than re-scanned on every
/// registration.
pub(crate) fn needs_direct_positional_placeholder_die_from_flag(
    original_param_defs_is_empty: bool,
    use_positional: bool,
    effective_param_defs: &mut Vec<ParamDef>,
) -> bool {
    if !original_param_defs_is_empty || !use_positional {
        return false;
    }
    if !effective_param_defs.iter().any(|pd| pd.name == "@_") {
        let insert_pos = effective_param_defs
            .iter()
            .position(|pd| pd.name.starts_with('%') && pd.slurpy)
            .unwrap_or(effective_param_defs.len());
        effective_param_defs.insert(insert_pos, implicit_method_positional_slurpy_param());
    }
    true
}

/// Scan a signature-less routine body for a bare `@_`/`%_` read, returning
/// `(uses_positional, uses_named)`. Used to decide whether to synthesize the
/// implicit `*@_`/`*%_` slurpies a signature-less sub/method body needs.
///
/// The walk (ADR-0137 visitor) reaches every position of the body, blocks
/// and closures included, but not a nested routine or package declaration,
/// which has `@_`/`%_` of its own.
// Cost: O(n), n = size of `stmts` outside nested routine declarations.
pub(crate) fn auto_signature_uses(stmts: &[Stmt]) -> (bool, bool) {
    #[derive(Default)]
    struct ImplicitArgsScan {
        positional: bool,
        named: bool,
    }
    impl<'ast> crate::ast_visit::Visit<'ast> for ImplicitArgsScan {
        fn visit_stmt(&mut self, stmt: &'ast Stmt) {
            // A nested routine, package or captured nested method binds its
            // own `@_`/`%_`.
            if !crate::ast::scope_scan::is_scope_declaration(stmt)
                && !matches!(stmt, Stmt::NestedMethodCapture { .. })
            {
                crate::ast_visit::walk_stmt(self, stmt);
            }
        }
        fn visit_name(&mut self, name: &str, kind: crate::ast_visit::NameKind) {
            match kind {
                crate::ast_visit::NameKind::ArrayVar if name == "_" => self.positional = true,
                crate::ast_visit::NameKind::HashVar if name == "_" => self.named = true,
                _ => {}
            }
        }
    }
    let mut scan = ImplicitArgsScan::default();
    crate::ast_visit::walk_stmts(&mut scan, stmts);
    (scan.positional, scan.named)
}
