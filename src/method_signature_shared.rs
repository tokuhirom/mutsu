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

/// The implicit `*@_` positional slurpy inserted into a signature-less
/// method body when [`auto_signature_uses`] detects a bare `@_` read.
pub(crate) fn implicit_method_positional_slurpy_param() -> ParamDef {
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

/// Auto-detect bare `@_`/`%_` usage in a signature-less method body and, if
/// a positional `@_` read is found, insert the implicit `*@_` slurpy before
/// any named `*%_` slurpy already present in `effective_param_defs`. Mirrors
/// Raku's implicit-arguments behavior for a method declared without an
/// explicit signature.
///
/// Only applies when the ORIGINAL declaration (`original_param_defs_is_empty`)
/// had no signature at all — an explicit (even empty, `()`) signature opts
/// out, matching `class_body_method_decl`'s `decl.param_defs.is_empty()`
/// guard. `effective_param_defs` is the vector already produced by
/// [`effective_method_param_defs`] (so its own `*%_` insertion, if any, is
/// visible to the insertion-position search below).
pub(crate) fn apply_auto_positional_slurpy(
    original_param_defs_is_empty: bool,
    body: &[Stmt],
    effective_param_defs: &mut Vec<ParamDef>,
) {
    if !original_param_defs_is_empty {
        return;
    }
    let (use_positional, _) = auto_signature_uses(body);
    if !use_positional || effective_param_defs.iter().any(|pd| pd.name == "@_") {
        return;
    }
    let insert_pos = effective_param_defs
        .iter()
        .position(|pd| pd.name.starts_with('%') && pd.slurpy)
        .unwrap_or(effective_param_defs.len());
    effective_param_defs.insert(insert_pos, implicit_method_positional_slurpy_param());
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
