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

use crate::ast::{CallArg, Expr, ParamDef, Stmt};
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
        name: "%_".to_string(),
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
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
        name: "@_".to_string(),
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
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
pub(crate) fn auto_signature_uses(stmts: &[Stmt]) -> (bool, bool) {
    fn scan_stmt(stmt: &Stmt, positional: &mut bool, named: &mut bool) {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e, _) => {
                scan_expr(e, positional, named);
            }
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                scan_expr(expr, positional, named);
            }
            Stmt::Call { args, .. } => {
                for arg in args {
                    match arg {
                        CallArg::Positional(e) | CallArg::Slip(e) | CallArg::Invocant(e) => {
                            scan_expr(e, positional, named)
                        }
                        CallArg::Named { value: Some(e), .. } => scan_expr(e, positional, named),
                        CallArg::Named { value: None, .. } => {}
                    }
                }
            }
            Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
                for e in es {
                    scan_expr(e, positional, named);
                }
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                scan_expr(cond, positional, named);
                for s in then_branch {
                    scan_stmt(s, positional, named);
                }
                for s in else_branch {
                    scan_stmt(s, positional, named);
                }
            }
            Stmt::While { cond, body, .. } => {
                scan_expr(cond, positional, named);
                for s in body {
                    scan_stmt(s, positional, named);
                }
            }
            Stmt::For { iterable, body, .. } => {
                scan_expr(iterable, positional, named);
                for s in body {
                    scan_stmt(s, positional, named);
                }
            }
            Stmt::Loop { body, .. }
            | Stmt::React { body }
            | Stmt::Block(body)
            | Stmt::SyntheticBlock(body)
            | Stmt::Default(body)
            | Stmt::Catch(body)
            | Stmt::Control(body)
            | Stmt::RoleDecl { body, .. }
            | Stmt::Phaser { body, .. } => {
                for s in body {
                    scan_stmt(s, positional, named);
                }
            }
            Stmt::Whenever { supply, body, .. } => {
                scan_expr(supply, positional, named);
                for s in body {
                    scan_stmt(s, positional, named);
                }
            }
            Stmt::Given { topic, body, .. } => {
                scan_expr(topic, positional, named);
                for s in body {
                    scan_stmt(s, positional, named);
                }
            }
            Stmt::When { cond, body } => {
                scan_expr(cond, positional, named);
                for s in body {
                    scan_stmt(s, positional, named);
                }
            }
            Stmt::Let { value, index, .. } => {
                if let Some(v) = value {
                    scan_expr(v, positional, named);
                }
                if let Some(i) = index {
                    scan_expr(i, positional, named);
                }
            }
            Stmt::TempMethodAssign {
                method_args, value, ..
            } => {
                for a in method_args {
                    scan_expr(a, positional, named);
                }
                scan_expr(value, positional, named);
            }
            Stmt::SubsetDecl {
                predicate: Some(p), ..
            } => {
                scan_expr(p, positional, named);
            }
            _ => {}
        }
    }

    fn scan_expr(expr: &Expr, positional: &mut bool, named: &mut bool) {
        match expr {
            Expr::ArrayVar(name) if name == "_" => *positional = true,
            Expr::HashVar(name) if name == "_" => *named = true,
            Expr::Binary { left, right, .. }
            | Expr::HyperOp { left, right, .. }
            | Expr::MetaOp { left, right, .. } => {
                scan_expr(left, positional, named);
                scan_expr(right, positional, named);
            }
            Expr::Unary { expr, .. }
            | Expr::PostfixOp { expr, .. }
            | Expr::AssignExpr { expr, .. }
            | Expr::ZenSlice(expr)
            | Expr::Reduction { expr, .. } => scan_expr(expr, positional, named),
            Expr::Exists { target, arg, .. } => {
                scan_expr(target, positional, named);
                if let Some(a) = arg {
                    scan_expr(a, positional, named);
                }
            }
            Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
                scan_expr(target, positional, named);
                for a in args {
                    scan_expr(a, positional, named);
                }
            }
            Expr::DynamicMethodCall {
                target,
                name_expr,
                args,
                ..
            }
            | Expr::HyperMethodCallDynamic {
                target,
                name_expr,
                args,
                ..
            } => {
                scan_expr(target, positional, named);
                scan_expr(name_expr, positional, named);
                for a in args {
                    scan_expr(a, positional, named);
                }
            }
            Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
                for a in args {
                    scan_expr(a, positional, named);
                }
            }
            Expr::CallOn { target, args } => {
                scan_expr(target, positional, named);
                for a in args {
                    scan_expr(a, positional, named);
                }
            }
            Expr::Index { target, index, .. } => {
                scan_expr(target, positional, named);
                scan_expr(index, positional, named);
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                scan_expr(cond, positional, named);
                scan_expr(then_expr, positional, named);
                scan_expr(else_expr, positional, named);
            }
            Expr::ArrayLiteral(es)
            | Expr::BracketArray(es, _)
            | Expr::StringInterpolation(es)
            | Expr::CaptureLiteral(es) => {
                for e in es {
                    scan_expr(e, positional, named);
                }
            }
            Expr::InfixFunc { left, right, .. } => {
                scan_expr(left, positional, named);
                for e in right {
                    scan_expr(e, positional, named);
                }
            }
            Expr::Block(stmts)
            | Expr::AnonSub { body: stmts, .. }
            | Expr::AnonSubParams { body: stmts, .. }
            | Expr::Gather(stmts) => {
                for s in stmts {
                    scan_stmt(s, positional, named);
                }
            }
            Expr::DoBlock { body, .. } => {
                for s in body {
                    scan_stmt(s, positional, named);
                }
            }
            Expr::DoStmt(stmt) => scan_stmt(stmt, positional, named),
            Expr::Lambda { body, .. } => {
                for s in body {
                    scan_stmt(s, positional, named);
                }
            }
            Expr::Try { body, catch } => {
                for s in body {
                    scan_stmt(s, positional, named);
                }
                if let Some(c) = catch {
                    for s in c {
                        scan_stmt(s, positional, named);
                    }
                }
            }
            Expr::IndirectCodeLookup { package, .. } => scan_expr(package, positional, named),
            Expr::SymbolicDeref { expr, .. } => scan_expr(expr, positional, named),
            Expr::Hash(pairs) => {
                for (_, value) in pairs {
                    if let Some(v) = value {
                        scan_expr(v, positional, named);
                    }
                }
            }
            _ => {}
        }
    }

    let mut positional = false;
    let mut named = false;
    for stmt in stmts {
        scan_stmt(stmt, &mut positional, &mut named);
    }
    (positional, named)
}
