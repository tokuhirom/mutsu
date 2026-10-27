//! Binding of the implicit aggregate spellings `@_` and `%_` at the model boundary.
//!
//! The first unbound use declares a placeholder. Later uses, including nested
//! blocks, reference that lexical. Explicit parameters bind the name before
//! conversion; a method's implicit named slurpy is resolved during lowering.

use std::cell::Cell;

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::ast_visit::Visit;
use crate::value::{RuntimeError, Value, ValueView};

const ARRAY: u8 = 1;
const HASH: u8 = 2;

thread_local! {
    static BOUND: Cell<u8> = const { Cell::new(0) };
    static IN_METHOD: Cell<bool> = const { Cell::new(false) };
}

/// Restore the enclosing conversion's bindings on every exit path.
pub(super) struct Scope(u8);

impl Scope {
    // Cost: O(1).
    pub(super) fn reset() -> Self {
        Self(BOUND.replace(0))
    }

    // Cost: O(p), p = parameters.
    pub(super) fn parameters(params: &[ParamDef]) -> Self {
        let declared = params.iter().fold(0, |bits, p| bits | name_bit(&p.name));
        let previous = BOUND.get();
        BOUND.set(previous | declared);
        Self(previous)
    }

    // Cost: O(n), n = nodes in the block's own lexical scope.
    pub(super) fn block(body: &[Stmt]) -> Self {
        struct Declarations(u8);
        impl<'ast> Visit<'ast> for Declarations {
            fn visit_stmt(&mut self, stmt: &'ast Stmt) {
                if let Stmt::VarDecl { name, .. } = stmt {
                    self.0 |= name_bit(name);
                }
                crate::ast::scope_scan::walk_stmt_own_scope(self, stmt);
            }
            fn visit_expr(&mut self, expr: &'ast Expr) {
                if !crate::ast::scope_scan::opens_own_scope(expr) {
                    crate::ast_visit::walk_expr(self, expr);
                }
            }
        }
        let previous = BOUND.get();
        let mut declared = Declarations(previous);
        for stmt in body {
            declared.visit_stmt(stmt);
        }
        BOUND.set(declared.0);
        Self(previous)
    }
}

impl Drop for Scope {
    fn drop(&mut self) {
        BOUND.set(self.0);
    }
}

/// Rakudo leaves implicit method `%_` reads as unresolved placeholder nodes.
/// Resolve those reads against the method's named-argument binding on lowering.
pub(super) struct MethodScope(bool);

impl MethodScope {
    // Cost: O(1).
    pub(super) fn enter(method: bool) -> Self {
        Self(IN_METHOD.replace(method))
    }
}

impl Drop for MethodScope {
    fn drop(&mut self) {
        IN_METHOD.set(self.0);
    }
}

// Cost: O(1).
fn name_bit(name: &str) -> u8 {
    match name {
        "@_" => ARRAY,
        "%_" => HASH,
        _ => 0,
    }
}

/// A statement block is invoked with no arguments. Keep its inlined body,
/// but declare fresh empty aggregates for its own slurpy placeholders rather
/// than reading the caller's arguments. Ordinary lexical reads stay captures.
// Cost: O(n), n = nodes in the block's own placeholder scope.
pub(super) fn lower_statement_block(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let mut body = super::lower::lower_block(node)?;
    let mut declared = declarations(super::lower::named_child(node, "body")?, 0)?;
    if IN_METHOD.get() {
        declared &= !HASH;
    }
    if !super::lower::bool_field(node, "may-have-signature")? {
        let placeholders = crate::ast::collect_placeholders_shallow(&body);
        let aggregate = if declared & ARRAY != 0 {
            Some("@_")
        } else if declared & HASH != 0 {
            Some("%_")
        } else {
            None
        };
        if let Some(name) = aggregate.or_else(|| {
            placeholders
                .iter()
                .find(|name| name_bit(name) == 0 || declared & name_bit(name) != 0)
                .map(String::as_str)
        }) {
            return Err(RuntimeError::from_exception_value(
                crate::method_signature_shared::placeholder_scope_error("block", name),
            ));
        }
    }
    let bindings = [(ARRAY, "@_"), (HASH, "%_")]
        .into_iter()
        .filter(|(bit, _)| declared & bit != 0)
        .map(|(_, name)| Stmt::VarDecl {
            anonymous: false,
            name: name.to_string(),
            expr: Expr::ArrayLiteral(Vec::new()),
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            has_initializer: false,
            where_constraint: None,
        });
    body.splice(0..0, bindings);
    Ok(Stmt::Block(body))
}

/// True only for the use that introduces the implicit binding in this scope.
// Cost: O(1).
pub(super) fn declares_placeholder(array: bool) -> bool {
    let bit = if array { ARRAY } else { HASH };
    let previous = BOUND.get();
    BOUND.set(previous | bit);
    previous & bit == 0
}

/// Keep explicit model declarations distinct from ordinary lexical reads.
/// Inferred parameters remain eligible for the compiler's outer-lexical capture;
/// an unbound placeholder declaration binds a fresh parameter. A method already
/// supplies an implicit named slurpy, so its unresolved hash reads capture it.
// Cost: O(n), n = nodes in the block's own placeholder scope.
pub(super) fn lower_block_value(node: &RakuAstNode, body: Vec<Stmt>) -> Result<Expr, RuntimeError> {
    let mut declarations = declarations(super::lower::named_child(node, "body")?, 0)?;
    if IN_METHOD.get() {
        declarations &= !HASH;
    }
    let mut closure = crate::ast::make_anon_sub(body);
    if let Expr::AnonSubParams { param_defs, .. } = &mut closure {
        for param in param_defs {
            if declarations & name_bit(&param.name) != 0 {
                param.block_param = false;
            }
        }
    }
    if let Expr::AnonSub { doc, .. } = &mut closure {
        *doc = super::declarator_docs::slot(node).unwrap_or_default();
    }
    Ok(closure)
}

// Cost: O(n), n = nodes before nested placeholder-scope boundaries; depth bounded by the stack (128 unguarded).
fn declarations(node: &RakuAstNode, depth: usize) -> Result<u8, RuntimeError> {
    if super::nesting::exceeded(depth, 128) {
        return Err(RuntimeError::new(
            "RakuAST: placeholder declaration nesting limit exceeded".to_string(),
        ));
    }
    match node.class {
        RakuAstClass::VarDeclarationPlaceholderSlurpyArray => return Ok(ARRAY),
        RakuAstClass::VarDeclarationPlaceholderSlurpyHash => return Ok(HASH),
        RakuAstClass::Block
        | RakuAstClass::PointyBlock
        | RakuAstClass::Sub
        | RakuAstClass::Method
        | RakuAstClass::Submethod
        | RakuAstClass::Class
        | RakuAstClass::Role
        | RakuAstClass::Grammar
        | RakuAstClass::Module
        | RakuAstClass::Package
        | RakuAstClass::RegexDeclaration
        | RakuAstClass::TokenDeclaration
        | RakuAstClass::RuleDeclaration => return Ok(0),
        _ => {}
    }
    let mut found = 0;
    for field in &node.fields {
        match &field.value {
            RakuAstFieldValue::Node(value) => found |= child_declarations(value, depth + 1)?,
            RakuAstFieldValue::List(values) => {
                for value in values {
                    found |= child_declarations(value, depth + 1)?;
                }
            }
            RakuAstFieldValue::Adverb(_) => {}
        }
    }
    Ok(found)
}

// Cost: O(n), n = nodes in the child subtree before placeholder-scope boundaries.
fn child_declarations(value: &Value, depth: usize) -> Result<u8, RuntimeError> {
    match value.view() {
        ValueView::RakuAst(node) => declarations(node, depth),
        _ => Ok(0),
    }
}

#[cfg(test)]
mod tests {
    use super::super::convert::node_field;
    use super::*;

    #[test]
    fn nested_scope_restores_first_use_bindings() {
        let _root = Scope::reset();
        {
            let _inner = Scope::block(&[]);
            assert!(declares_placeholder(false));
            assert!(!declares_placeholder(false));
        }
        assert!(declares_placeholder(false));
        {
            let _inner = Scope::block(&[]);
            assert!(!declares_placeholder(false));
        }
        assert!(!declares_placeholder(false));
    }

    #[test]
    fn nested_routine_declarations_do_not_bind_the_outer_block() {
        let _root = Scope::reset();
        let (body, _) = crate::parser::parse_program("my &inner = sub { my %_ = (); };")
            .expect("the nested routine parses");
        let _block = Scope::block(&body);
        assert!(declares_placeholder(false));
    }

    #[test]
    fn declaration_scan_bounds_model_nesting() {
        let mut node = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::VarDeclarationPlaceholderSlurpyHash,
            fields: Vec::new(),
        };
        for _ in 0..128 {
            node = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::StatementExpression,
                fields: vec![node_field(Some("expression"), node)],
            };
        }
        assert!(declarations(&node, 0).is_err());
    }
}
