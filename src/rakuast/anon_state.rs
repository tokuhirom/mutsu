//! Anonymous variables (`$`, `@`, `%`) read back from RakuAST.
//!
//! `VarDeclaration::Anonymous` carries no name; the parser gives each bare `$`
//! a minted one and declares it as a `state` at the top of the enclosing block
//! (see `ast::anon_state`). Lowering a block therefore keeps a frame: every
//! anonymous scalar minted while its statements are lowered is declared at the
//! block's top, and a body nested below a routine body gets the per-call
//! spelling, exactly as `parser::stmt::simple::anon_state_is_per_call` decides.

use std::cell::RefCell;

use super::lower::{leaf_str, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::value::RuntimeError;

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

/// Whether an anonymous declaration is the `state` default of a bare `$`
/// rather than a declared `my $` (no `scope`, rakudo's default).
// Cost: O(f), f = fields of the node.
fn is_my(node: &RakuAstNode) -> Result<bool, RuntimeError> {
    if node.fields.iter().all(|f| f.name != Some("scope")) {
        return Ok(true);
    }
    match leaf_str(node, "scope")?.as_str() {
        "state" => Ok(false),
        "my" => Ok(true),
        _ => Err(unsupported(node)),
    }
}

/// Whether a node needs an explicit declaration rather than a fresh bare-sigil
/// state cell. Initializers and type constraints are lowered once by the shared
/// variable-declaration lowering.
fn parts(node: &RakuAstNode) -> Result<(String, bool), RuntimeError> {
    is_my(node)?;
    Ok((
        leaf_str(node, "sigil")?,
        node.fields
            .iter()
            .any(|field| matches!(field.name, Some("initializer" | "type"))),
    ))
}

/// An anonymous variable in expression position: `$++`, `@.push`, `state $ = 0`.
// Cost: O(n), n = size of the initializer.
pub(super) fn lower_term(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let (sigil, explicit) = parts(node)?;
    if is_my(node)? || explicit {
        return if sigil == "$" {
            Ok(Expr::DoStmt(Box::new(super::lower::lower_var_decl(node)?)))
        } else {
            Err(unsupported(node))
        };
    }
    match sigil.as_str() {
        "$" => Ok(Expr::Var(mint_scalar())),
        "@" => Ok(Expr::ArrayVar(crate::parser::fresh_anon_array_name())),
        "%" => Ok(Expr::HashVar("__ANON_HASH__".to_string())),
        _ => Err(unsupported(node)),
    }
}

/// An anonymous variable as a statement of its own: `state $ = 0;`.
// Cost: O(n), n = size of the initializer.
pub(super) fn lower_statement(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let (sigil, explicit) = parts(node)?;
    if sigil == "$" && (is_my(node)? || explicit) {
        super::lower::lower_var_decl(node)
    } else {
        Ok(Stmt::Expr(lower_term(node)?))
    }
}

/// The name an assignment to an anonymous scalar (`$ = 5`) writes.
// Cost: O(depth).
pub(super) fn assign_target(node: &RakuAstNode) -> Result<String, RuntimeError> {
    if is_my(node)? {
        return Err(unsupported(node));
    }
    match parts(node)? {
        (sigil, false) if sigil == "$" => Ok(mint_scalar()),
        _ => Err(unsupported(node)),
    }
}
