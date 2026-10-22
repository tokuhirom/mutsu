//! Construct families the parser builds in their RakuAST shape
//! (ADR-10723 Stage 2, §4 "Typed nodes vs. the uniform `RakuAstNode`").
//!
//! A migrated family's parse function builds a typed node shaped like the
//! RakuAST node (`Statement::If` keeps its condition, its `then` block, its
//! `elsif`/`orwith` clauses and its `else` block) and records the source
//! positions Rakudo gives those nodes. Children of a family that has not
//! migrated yet stay ordinary `Expr`/`Stmt` values.
//!
//! An executing parse hands the node to `rakuast::lower` where it is built, so
//! the compiler and every analysis see the tree they always saw. A parse whose
//! tree only the RakuAST conversion reads (`ast::spelled::keeping`) keeps the
//! node in [`Stmt::Ast`]; `.AST` renders it, positions included, and the
//! `MUTSU_RAKUAST` round trip lowers it through the same function.

use super::{Expr, ParamDef, SrcSpan, Stmt};

/// A statement-level construct kept in its RakuAST shape.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ParsedStmt {
    /// `if` / `unless` / `with` / `without` and their clause chain.
    Conditional(Conditional),
}

/// Which keyword opened a conditional statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ConditionalKind {
    If,
    Unless,
    With,
    Without,
}

/// Which keyword opened a continuation clause.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ClauseKind {
    Elsif,
    Orwith,
}

/// `Statement::If` / `::Unless` / `::With` / `::Without`.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Conditional {
    pub(crate) kind: ConditionalKind,
    /// The condition as written: no `!` for `unless`, no `.defined` test for
    /// `with`.
    pub(crate) condition: Expr,
    /// The `then` block (`body` for `unless` / `without`).
    pub(crate) then: ParsedBlock,
    /// The `elsif` / `orwith` clauses, in source order.
    pub(crate) clauses: Vec<ConditionalClause>,
    /// The `else` block.
    pub(crate) else_block: Option<ParsedBlock>,
}

/// `Statement::Elsif` / `Statement::Orwith`.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct ConditionalClause {
    pub(crate) kind: ClauseKind,
    pub(crate) condition: Expr,
    pub(crate) then: ParsedBlock,
}

/// The parameters written in front of a block, as the parser read them.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum BlockParams {
    /// A plain `{ ... }`.
    None,
    /// `-> PARAMS { ... }` after `if` / `elsif` / `else` / `unless`: the
    /// parameter list (empty for `-> { }`).
    List(Vec<ParamDef>),
    /// `-> PARAM { ... }` after `with` / `orwith` / `without`, or after the
    /// `else` of a `with` with no clauses: the single topic parameter (both
    /// `None` for `-> { }`).
    Topic {
        name: Option<String>,
        def: Option<ParamDef>,
    },
}

/// A `Block` / `PointyBlock` with its `Blockoid` and `StatementList`.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct ParsedBlock {
    pub(crate) params: BlockParams,
    pub(crate) body: Vec<Stmt>,
    /// The whole block: from `{`, or from `->` for a pointy block, through `}`.
    pub(crate) span: SrcSpan,
    /// The braces, `{` through `}` (the `Blockoid`).
    pub(crate) blockoid: SrcSpan,
    /// The statements: from the first statement to the end of the last one,
    /// its `;` included; empty just after `{` when there is none.
    pub(crate) list: SrcSpan,
}
