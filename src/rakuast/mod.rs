//! RakuAST — a reflection/model layer over the internal AST (ADR-0011).
//!
//! Phase 1: read-only introspection. `Str.AST` parses source, converts the
//! internal `Stmt`/`Expr` AST into a [`RakuAstNode`] tree (wrapped in
//! `Value::RakuAst`), whose `.gist`/`.raku`/`.Str` renders the
//! `RakuAST::*.new(...)` constructor form and whose `.^name` returns the
//! printed class name.
//!
//! RakuAST is deliberately NOT mutsu's compiler IR — it is a model layer that
//! maps to/from the internal AST. See docs/adr/0011 for the full design and
//! phasing (construction, EVAL, macros are later phases).

mod convert;
mod render;

use crate::value::{RuntimeError, Value};

/// A single RakuAST node: its class plus ordered fields. Immutable tree.
#[derive(Debug, Clone, PartialEq)]
pub struct RakuAstNode {
    pub class: RakuAstClass,
    pub fields: Vec<RakuAstField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RakuAstField {
    /// `None` => positional `.new()` argument; `Some` => named argument (and,
    /// in Phase 3, the accessor name).
    pub name: Option<&'static str>,
    pub value: RakuAstFieldValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RakuAstFieldValue {
    /// A child node (`Value::RakuAst`) or a leaf literal (`Int`/`Rat`/`Str`).
    Node(Value),
    /// A parenthesised, trailing-comma list of child nodes (e.g. `segments`).
    List(Vec<Value>),
}

/// Every known RakuAST node kind. Exhaustive `match` on this in the converter
/// and renderer (and, later, the lowerer) keeps the layer honest as it grows —
/// adding a kind is a compile error until every site handles it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RakuAstClass {
    StatementList,
    StatementExpression,
    IntLiteral,
    RatLiteral,
    StrLiteral,
    QuotedString,
    CallName,
    CallNameWithoutParentheses,
    Name,
    ArgList,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Constructor {
    New,
    FromIdentifier,
}

impl RakuAstClass {
    /// Printed class name (also what `.^name` returns).
    pub fn printed_name(self) -> &'static str {
        use RakuAstClass::*;
        match self {
            StatementList => "RakuAST::StatementList",
            StatementExpression => "RakuAST::Statement::Expression",
            IntLiteral => "RakuAST::IntLiteral",
            RatLiteral => "RakuAST::RatLiteral",
            StrLiteral => "RakuAST::StrLiteral",
            QuotedString => "RakuAST::QuotedString",
            CallName => "RakuAST::Call::Name",
            CallNameWithoutParentheses => "RakuAST::Call::Name::WithoutParentheses",
            Name => "RakuAST::Name",
            ArgList => "RakuAST::ArgList",
        }
    }

    pub fn constructor(self) -> Constructor {
        match self {
            RakuAstClass::Name => Constructor::FromIdentifier,
            _ => Constructor::New,
        }
    }

    /// Field width for aligning named `key => value` fields. raku's gist pads to
    /// the max length over ALL of the class's declared attributes — including
    /// ones omitted when at their default (e.g. `QuotedString` pads `segments`
    /// to 10 to align with its unshown `processors`). These constants are
    /// captured from raku's output; 0 = the class has no named fields.
    pub fn named_align_width(self) -> usize {
        use RakuAstClass::*;
        match self {
            StatementExpression => 10,                  // "expression"
            QuotedString => 10,                         // "processors" (unshown) > "segments"
            CallName | CallNameWithoutParentheses => 4, // "name" / "args"
            _ => 0,
        }
    }
}

/// Entry point for `Str.AST`: parse the source, convert, wrap in `Value::RakuAst`.
pub fn str_dot_ast(source: &str) -> Result<Value, RuntimeError> {
    let (stmts, _finish) = crate::parse_dispatch::parse_source(source)?;
    let node = convert::statement_list(&stmts)?;
    Ok(Value::rakuast(Box::new(node)))
}

/// `.gist` / `.raku` / `.Str` of a RakuAST node.
pub fn node_gist(node: &RakuAstNode) -> String {
    render::render_node(node, 0)
}
