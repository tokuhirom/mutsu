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
    /// `given TOPIC BLOCK` (`Statement::Given`); the block's topic parameter,
    /// if any, is `BlockParams::Topic`.
    Given { topic: Expr, block: ParsedBlock },
    /// `when MATCHER BLOCK` (`Statement::When`).
    When { condition: Expr, block: ParsedBlock },
    /// `default BLOCK` (`Statement::Default`).
    Default(ParsedBlock),
    /// `while COND BLOCK` / `until COND BLOCK` (`Statement::Loop::While` /
    /// `::Until`).
    WhileLoop(WhileLoop),
    /// A bare `{ ... }` statement (`Statement::Expression` over a `Block`).
    BareBlock(ParsedBlock),
    /// `loop`, C-style or bare, and `repeat ... while` / `until`
    /// (`Statement::Loop`, `::Loop::RepeatWhile` / `::RepeatUntil`).
    Loop(LoopStmt),
    /// `for LIST BLOCK` (`Statement::For`). `Stmt::For` already keeps what
    /// the source wrote, so the node is that statement plus the positions of
    /// its block.
    For { stmt: Box<Stmt>, block: BlockSpans },
    /// A binding declaration, `my $x := EXPR` / `my @a := EXPR` / `my %h :=
    /// EXPR` (`VarDeclaration::Simple` with `Initializer::Bind`): the
    /// `Stmt::VarDecl` as written. `rakuast::lower` wraps it in the
    /// bookkeeping statements the compiler needs (`ast::bind_decl::expand`).
    BindDecl(Box<Stmt>),
    /// `my %h does Role`, `my @a does Role = 1, 2`: the declaration (a
    /// `Stmt::VarDecl`), the role as written and the `=` initializer.
    /// `rakuast::lower` mixes the role in between the two
    /// (`ast::var_does::expand`).
    DoesDecl {
        decl: Box<Stmt>,
        role: Expr,
        init: Option<Expr>,
    },
    /// `my $x will begin { ... }`: the declaration (a `Stmt::VarDecl`) and the
    /// `will PHASE BLOCK` traits as written (kind and body). `rakuast::lower`
    /// expands each into a phaser after the declaration
    /// (`parser::wrap_with_will_phasers`).
    WillDecl {
        decl: Box<Stmt>,
        phasers: Vec<(super::PhaserKind, Vec<Stmt>)>,
    },
    /// A statement with statement modifiers (`Statement::Expression`'s
    /// `condition-modifier` and `loop-modifier`).
    Modified(Modified),
}

/// `STMT if COND`, `STMT for LIST`, `STMT if COND for LIST`: the statement as
/// written and its modifiers. A statement takes at most one condition
/// modifier and one loop modifier, the condition applying first.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Modified {
    pub(crate) stmt: Box<Stmt>,
    pub(crate) condition: Option<StatementModifier>,
    pub(crate) loop_: Option<StatementModifier>,
    /// The modified statement, from its first character to its last; the
    /// start is filled in by the statement parser, which knows it.
    pub(crate) body: SrcSpan,
}

/// One statement modifier: its keyword, its operand as written (no `!` for
/// `unless` / `until`) and where it was written, from the keyword to the end
/// of the operand.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct StatementModifier {
    pub(crate) kind: ModifierKind,
    pub(crate) expr: Expr,
    pub(crate) span: SrcSpan,
}

/// Which keyword a statement modifier was written with.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ModifierKind {
    If,
    Unless,
    When,
    With,
    Without,
    For,
    While,
    Until,
    Given,
}

impl ModifierKind {
    /// The modifier written as `keyword`, if it is one.
    // Cost: O(1).
    pub(crate) fn from_keyword(keyword: &str) -> Option<Self> {
        Some(match keyword {
            "if" => Self::If,
            "unless" => Self::Unless,
            "when" => Self::When,
            "with" => Self::With,
            "without" => Self::Without,
            "for" => Self::For,
            "while" => Self::While,
            "until" => Self::Until,
            "given" => Self::Given,
            _ => return None,
        })
    }

    /// Whether this is a loop modifier (`for` / `while` / `until` / `given`)
    /// rather than a condition modifier.
    // Cost: O(1).
    pub(crate) fn is_loop(self) -> bool {
        matches!(self, Self::For | Self::While | Self::Until | Self::Given)
    }
}

/// Where a block was written: the block (from `->` when pointy), its braces and
/// its statements. See [`ParsedBlock`].
#[derive(Debug, Clone, Copy, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct BlockSpans {
    pub(crate) span: SrcSpan,
    pub(crate) blockoid: SrcSpan,
    pub(crate) list: SrcSpan,
}

/// Where a routine declaration (`Sub`, `Method`, `Submethod`) was written,
/// below the statement it is: its name, its signature and its block. Kept on
/// `Stmt::SubDecl` / `Stmt::MethodDecl`, which already hold what the source
/// wrote, so the declaration is its own typed node (ADR-10723 §4).
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct RoutineSpans {
    /// The declared name (`foo`, `p` of `!p`).
    pub(crate) name: SrcSpan,
    /// From the first parameter to the end of the last; an empty `( )` sits at
    /// its `)`, and a routine with no parentheses has none.
    pub(crate) signature: SrcSpan,
    pub(crate) block: BlockSpans,
    /// The traits written after the signature, in source order, one per
    /// trait the `.AST` parse records (`ast::routine_trait`).
    #[serde(default)]
    pub(crate) traits: Vec<TraitSpans>,
}

/// Where a trait was written (`is copy`, `returns Int`): all of it, and the
/// name or type it names.
#[derive(Debug, Clone, Copy, Default, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct TraitSpans {
    pub(crate) whole: SrcSpan,
    pub(crate) name: SrcSpan,
}

/// Where a package declaration (`class`, `grammar`, `role`) was written
/// below its statement: its name and its block.
#[derive(Debug, Clone, Copy, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct PackageSpans {
    pub(crate) name: SrcSpan,
    pub(crate) block: BlockSpans,
}

/// Where a type declaration that is not a package (`enum`, `subset`) was
/// written below its statement: its name, and a subset's `of` trait.
#[derive(Debug, Clone, Copy, Default, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct TypeDeclSpans {
    pub(crate) name: SrcSpan,
    /// `of TYPE` (a subset's base type as written); none when not written.
    pub(crate) of: TraitSpans,
    /// `is export` on a subset; none when not written.
    #[serde(default)]
    pub(crate) export: TraitSpans,
}

/// Where a parameter was written: all of it (type, traits and default
/// included) and its target variable.
///
/// Like `SrcSpan`'s `Hash`, its `Debug` leaves the positions out: the runtime
/// still tells two signatures apart by their `Debug` text in places (a forward
/// declaration and its definition, a role's re-registered method), and a
/// signature is the same wherever it was written.
#[derive(Clone, Default, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct ParamSpans {
    pub(crate) param: SrcSpan,
    pub(crate) target: SrcSpan,
    /// The nominal type, smiley included (`Int:D`).
    pub(crate) type_: SrcSpan,
    /// The `is` traits, in the order of `ParamDef::traits`.
    #[serde(default)]
    pub(crate) traits: Vec<TraitSpans>,
}

impl std::fmt::Debug for ParamSpans {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ParamSpans")
    }
}

/// Which loop a [`LoopStmt`] is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum LoopKind {
    /// `loop { }` / `loop (init; cond; step) { }`.
    Loop,
    RepeatWhile,
    RepeatUntil,
}

/// `loop` / `repeat`. A `repeat` keeps its condition as written (no `!` for
/// `until`) and its pointy parameter as the block's `BlockParams::Topic`.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct LoopStmt {
    pub(crate) kind: LoopKind,
    pub(crate) init: Option<Box<Stmt>>,
    pub(crate) condition: Option<Expr>,
    pub(crate) step: Option<Expr>,
    pub(crate) block: ParsedBlock,
    pub(crate) label: Option<String>,
    /// Written as a term without `do` (`(while COND { ... })`); see
    /// `Stmt::While::is_bare_term`.
    pub(crate) bare_term: bool,
}

/// `while` / `until` with its optional label.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct WhileLoop {
    pub(crate) until: bool,
    /// The condition as written: no `!` for `until`.
    pub(crate) condition: Expr,
    pub(crate) block: ParsedBlock,
    pub(crate) label: Option<String>,
    /// Written as a term without `do` (`(while COND { ... })`); see
    /// `Stmt::While::is_bare_term`.
    pub(crate) bare_term: bool,
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
        def: Option<Box<ParamDef>>,
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

impl ParsedStmt {
    /// The statement a node of a family whose internal form is already its
    /// source form wraps (`for`): passes treat the node as that statement.
    // Cost: O(1).
    pub(crate) fn inner_stmt(&self) -> Option<&Stmt> {
        match self {
            ParsedStmt::For { stmt, .. } | ParsedStmt::BindDecl(stmt) => Some(stmt),
            _ => None,
        }
    }

    /// Whether this is a `while` / `until` / `loop` / `repeat` written as a
    /// term without a `do`.
    // Cost: O(1).
    pub(crate) fn is_bare_loop_term(&self) -> bool {
        match self {
            ParsedStmt::WhileLoop(w) => w.bare_term,
            ParsedStmt::Loop(l) => l.bare_term,
            _ => false,
        }
    }

    /// Record that this loop is written as a term without a `do`.
    // Cost: O(1).
    pub(crate) fn mark_bare_loop_term(&mut self) {
        match self {
            ParsedStmt::WhileLoop(w) => w.bare_term = true,
            ParsedStmt::Loop(l) => l.bare_term = true,
            _ => {}
        }
    }

    /// [`ParsedStmt::inner_stmt`], mutably.
    // Cost: O(1).
    pub(crate) fn inner_stmt_mut(&mut self) -> Option<&mut Stmt> {
        match self {
            ParsedStmt::For { stmt, .. } | ParsedStmt::BindDecl(stmt) => Some(stmt),
            _ => None,
        }
    }

    /// The expressions that run in the enclosing scope: every condition, topic
    /// and matcher (none for a node with an [`inner_stmt`](Self::inner_stmt)).
    // Cost: O(c), c = clauses.
    pub(crate) fn headers(&self) -> Vec<&Expr> {
        match self {
            ParsedStmt::Conditional(c) => std::iter::once(&c.condition)
                .chain(c.clauses.iter().map(|clause| &clause.condition))
                .collect(),
            ParsedStmt::Given { topic, .. } => vec![topic],
            ParsedStmt::When { condition, .. } => vec![condition],
            ParsedStmt::WhileLoop(w) => vec![&w.condition],
            ParsedStmt::Loop(l) => l.condition.iter().chain(l.step.iter()).collect(),
            ParsedStmt::For { .. } | ParsedStmt::BindDecl(_) => Vec::new(),
            ParsedStmt::DoesDecl { role, init, .. } => std::iter::once(role).chain(init).collect(),
            ParsedStmt::WillDecl { .. } => Vec::new(),
            ParsedStmt::Modified(m) => m.modifiers().map(|modifier| &modifier.expr).collect(),
            ParsedStmt::Default(_) | ParsedStmt::BareBlock(_) => Vec::new(),
        }
    }

    /// Every block, in source order.
    // Cost: O(c), c = clauses.
    pub(crate) fn blocks(&self) -> Vec<&ParsedBlock> {
        match self {
            ParsedStmt::Conditional(c) => std::iter::once(&c.then)
                .chain(c.clauses.iter().map(|clause| &clause.then))
                .chain(c.else_block.iter())
                .collect(),
            ParsedStmt::Given { block, .. }
            | ParsedStmt::When { block, .. }
            | ParsedStmt::Default(block)
            | ParsedStmt::BareBlock(block) => vec![block],
            ParsedStmt::WhileLoop(w) => vec![&w.block],
            ParsedStmt::Loop(l) => vec![&l.block],
            ParsedStmt::For { .. }
            | ParsedStmt::BindDecl(_)
            | ParsedStmt::DoesDecl { .. }
            | ParsedStmt::WillDecl { .. }
            | ParsedStmt::Modified(_) => Vec::new(),
        }
    }
}

impl Modified {
    /// The modifiers, in the order they apply (condition, then loop).
    // Cost: O(1).
    pub(crate) fn modifiers(&self) -> impl Iterator<Item = &StatementModifier> {
        self.condition.iter().chain(self.loop_.iter())
    }
}

impl ParsedBlock {
    /// Where the block was written.
    // Cost: O(1).
    pub(crate) fn spans(&self) -> BlockSpans {
        BlockSpans {
            span: self.span,
            blockoid: self.blockoid,
            list: self.list,
        }
    }

    /// Whether the block was written with `->`.
    // Cost: O(1).
    pub(crate) fn is_pointy(&self) -> bool {
        !matches!(self.params, BlockParams::None)
    }
}

impl Stmt {
    /// The statement passes treat this one as: the wrapped statement of a
    /// node whose internal form is already its source form (`for`), else
    /// `self`.
    // Cost: O(1).
    pub(crate) fn peel_parsed(&self) -> &Stmt {
        match self {
            Stmt::Ast(node) => node.inner_stmt().unwrap_or(self),
            other => other,
        }
    }

    /// [`Stmt::peel_parsed`], mutably.
    // Cost: O(1).
    pub(crate) fn peel_parsed_mut(&mut self) -> &mut Stmt {
        if !matches!(self, Stmt::Ast(node) if node.inner_stmt().is_some()) {
            return self;
        }
        match self {
            Stmt::Ast(node) => node.inner_stmt_mut().expect("checked above"),
            _ => unreachable!("checked above"),
        }
    }
}
