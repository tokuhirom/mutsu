//! Anonymous variables (`$`, `@`, `%`) read back from RakuAST.
//!
//! `VarDeclaration::Anonymous` carries no name; the parser gives each bare `$`
//! a minted one and declares it as a `state` at the top of the enclosing block
//! (see `ast::anon_state`). Lowering a block therefore keeps a frame: every
//! anonymous scalar minted while its statements are lowered is declared at the
//! block's top, and a body nested below a routine body gets the per-call
//! spelling, exactly as `parser::stmt::simple::anon_state_is_per_call` decides.

use std::cell::RefCell;

use super::lower::{leaf_str, lower_expr, named_child, named_child_or_positional, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::value::{RuntimeError, Value};

struct Frame {
    is_routine_body: bool,
    names: Vec<String>,
}

thread_local! {
    static FRAMES: RefCell<Vec<Frame>> = const { RefCell::new(Vec::new()) };
    static NEXT_IS_ROUTINE: RefCell<bool> = const { RefCell::new(false) };
}

/// Mark the next body lowered by [`with_frame`] as a routine body.
// Cost: O(1).
pub(super) fn next_body_is_routine() {
    NEXT_IS_ROUTINE.with(|f| *f.borrow_mut() = true);
}

/// Lower one block body: `body` runs with a fresh frame, and the declarations
/// of the anonymous scalars it minted are put in front of the statements it
/// returns.
// Cost: O(n), n = statements in the body.
pub(super) fn with_frame<E>(body: impl FnOnce() -> Result<Vec<Stmt>, E>) -> Result<Vec<Stmt>, E> {
    let is_routine_body = NEXT_IS_ROUTINE.with(|f| std::mem::take(&mut *f.borrow_mut()));
    FRAMES.with(|f| {
        f.borrow_mut().push(Frame {
            is_routine_body,
            names: Vec::new(),
        })
    });
    let result = body();
    let frame = FRAMES.with(|f| f.borrow_mut().pop());
    let mut stmts = result?;
    if let Some(frame) = frame
        && !frame.names.is_empty()
    {
        let decls = frame
            .names
            .into_iter()
            .map(crate::ast::anon_state::implicit_decl);
        stmts.splice(0..0, decls);
    }
    Ok(stmts)
}

/// A fresh anonymous scalar of the block being lowered. A frame-less lowering
/// (a hand-built tree reaching an expression directly) gets an undeclared
/// name, which the runtime treats as the top-level parser's are treated.
// Cost: O(depth), depth = nesting of the lowered blocks.
pub(super) fn mint_scalar() -> String {
    FRAMES.with(|f| {
        let mut frames = f.borrow_mut();
        let per_call = frames
            .last()
            .is_some_and(|current| !current.is_routine_body)
            && frames
                .iter()
                .rev()
                .skip(1)
                .any(|frame| frame.is_routine_body);
        let name = crate::parser::fresh_anon_state_name(per_call);
        if let Some(current) = frames.last_mut() {
            current.names.push(name.clone());
        }
        name
    })
}

/// Whether `node` is the declaration of an anonymous variable.
// Cost: O(1).
pub(super) fn is_anonymous(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::VarDeclarationAnonymous
}

/// The sigil and the `= EXPR` of an anonymous declaration. Only the `state`
/// scope the converter renders lowers.
fn parts(node: &RakuAstNode) -> Result<(String, Option<Expr>), RuntimeError> {
    if leaf_str(node, "scope")? != "state" {
        return Err(unsupported(node));
    }
    let sigil = leaf_str(node, "sigil")?;
    let initializer = match node.fields.iter().find(|f| f.name == Some("initializer")) {
        None => None,
        Some(_) => {
            let init = named_child(node, "initializer")?;
            if init.class != RakuAstClass::InitializerAssign {
                return Err(unsupported(node));
            }
            Some(lower_expr(named_child_or_positional(init)?)?)
        }
    };
    Ok((sigil, initializer))
}

/// `state $ = EXPR` / `state $`: the declaration the parser builds for the
/// anonymous scalar, under its shared `__ANON_STATE__` name.
fn declaration(initializer: Option<Expr>) -> Stmt {
    let has_initializer = initializer.is_some();
    Stmt::VarDecl {
        name: "__ANON_STATE__".to_string(),
        expr: initializer.unwrap_or(Expr::Literal(Value::NIL)),
        type_constraint: None,
        is_state: true,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: if has_initializer {
            vec![("__has_initializer".to_string(), None)]
        } else {
            Vec::new()
        },
        where_constraint: None,
    }
}

/// An anonymous variable in expression position: `$++`, `@.push`, `state $ = 0`.
// Cost: O(n), n = size of the initializer.
pub(super) fn lower_term(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    match parts(node)? {
        (sigil, None) if sigil == "$" => Ok(Expr::Var(mint_scalar())),
        (sigil, None) if sigil == "@" => Ok(Expr::ArrayVar(crate::parser::fresh_anon_array_name())),
        (sigil, None) if sigil == "%" => Ok(Expr::HashVar("__ANON_HASH__".to_string())),
        (sigil, Some(init)) if sigil == "$" => Ok(Expr::DoStmt(Box::new(declaration(Some(init))))),
        _ => Err(unsupported(node)),
    }
}

/// An anonymous variable as a statement of its own: `state $ = 0;`.
// Cost: O(n), n = size of the initializer.
pub(super) fn lower_statement(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    match parts(node)? {
        (sigil, init) if sigil == "$" => Ok(declaration(init)),
        _ => Ok(Stmt::Expr(lower_term(node)?)),
    }
}

/// The name an assignment to an anonymous scalar (`$ = 5`) writes.
// Cost: O(depth).
pub(super) fn assign_target(node: &RakuAstNode) -> Result<String, RuntimeError> {
    match parts(node)? {
        (sigil, None) if sigil == "$" => Ok(mint_scalar()),
        _ => Err(unsupported(node)),
    }
}
