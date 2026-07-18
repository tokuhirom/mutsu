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
    /// A boolean colonpair adverb rendered as `:name` (e.g. `Assignment.new(:item)`).
    Adverb(&'static str),
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
    // Phase 2: variables, declarations, operators.
    VarLexical,
    VarDeclarationSimple,
    InitializerAssign,
    ApplyInfix,
    Infix,
    ApplyPrefix,
    Prefix,
    ApplyPostfix,
    Postfix,
    Assignment,
    CallMethod,
    // Phase 2 slice 23: quoted method names.
    CallQuotedMethod,
    // Phase 2 slice 26: hyper method calls.
    MetaPostfixHyper,
    // Phase 2 slice 3: blocks & pointy blocks.
    Block,
    Blockoid,
    PointyBlock,
    Signature,
    Parameter,
    ParameterTargetVar,
    // Phase 2 slice 4: conditionals and loops.
    StatementIf,
    StatementLoopWhile,
    StatementLoop,
    // Phase 2 slice 5: elsif chains.
    StatementElsif,
    // Phase 2 slice 6: for loops (implicit topic).
    StatementFor,
    // Phase 2 slice 7: named sub declarations.
    Sub,
    TypeSetting,
    // Phase 2 slice 8: C-style and repeat loops.
    StatementLoopRepeatWhile,
    // Phase 2 slice 9: `:=` binding and comma lists.
    ApplyListInfix,
    // Phase 2 slice 10: scoped/typed variable declarations.
    TypeSimple,
    // Phase 2 slice 20: definite types (`Int:D` / `Int:U`).
    TypeDefinedness,
    // Phase 2 slice 27: attribute build-time defaults.
    TraitWillBuild,
    // Phase 2 slice 21: parameterised types (`Array[Int]`).
    TypeParameterized,
    // Phase 2 slice 29: coercion types (`Int()`).
    TypeCoercion,
    // Phase 2 slice 13: class and method declarations.
    Class,
    Method,
    // Phase 2 slice 16: role declarations.
    Role,
    RoleBody,
    // Phase 2 slice 17: loop labels.
    Label,
    // Phase 2 slice 18: given/when/default.
    StatementGiven,
    StatementWhen,
    StatementDefault,
    // Phase 2 slice 19: ternary.
    Ternary,
    // Phase 2 slice 22: positional subscripts.
    SemiList,
    PostcircumfixArrayIndex,
    // Phase 2 slice 25: reduction metaoperator.
    TermReduce,
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
            VarLexical => "RakuAST::Var::Lexical",
            VarDeclarationSimple => "RakuAST::VarDeclaration::Simple",
            InitializerAssign => "RakuAST::Initializer::Assign",
            ApplyInfix => "RakuAST::ApplyInfix",
            Infix => "RakuAST::Infix",
            ApplyPrefix => "RakuAST::ApplyPrefix",
            Prefix => "RakuAST::Prefix",
            ApplyPostfix => "RakuAST::ApplyPostfix",
            Postfix => "RakuAST::Postfix",
            Assignment => "RakuAST::Assignment",
            CallMethod => "RakuAST::Call::Method",
            CallQuotedMethod => "RakuAST::Call::QuotedMethod",
            MetaPostfixHyper => "RakuAST::MetaPostfix::Hyper",
            Block => "RakuAST::Block",
            Blockoid => "RakuAST::Blockoid",
            PointyBlock => "RakuAST::PointyBlock",
            Signature => "RakuAST::Signature",
            Parameter => "RakuAST::Parameter",
            ParameterTargetVar => "RakuAST::ParameterTarget::Var",
            StatementIf => "RakuAST::Statement::If",
            StatementLoopWhile => "RakuAST::Statement::Loop::While",
            StatementLoop => "RakuAST::Statement::Loop",
            StatementElsif => "RakuAST::Statement::Elsif",
            StatementFor => "RakuAST::Statement::For",
            Sub => "RakuAST::Sub",
            TypeSetting => "RakuAST::Type::Setting",
            StatementLoopRepeatWhile => "RakuAST::Statement::Loop::RepeatWhile",
            ApplyListInfix => "RakuAST::ApplyListInfix",
            TypeSimple => "RakuAST::Type::Simple",
            TypeDefinedness => "RakuAST::Type::Definedness",
            TraitWillBuild => "RakuAST::Trait::WillBuild",
            TypeParameterized => "RakuAST::Type::Parameterized",
            TypeCoercion => "RakuAST::Type::Coercion",
            Class => "RakuAST::Class",
            Method => "RakuAST::Method",
            Role => "RakuAST::Role",
            RoleBody => "RakuAST::RoleBody",
            Label => "RakuAST::Label",
            StatementGiven => "RakuAST::Statement::Given",
            StatementWhen => "RakuAST::Statement::When",
            StatementDefault => "RakuAST::Statement::Default",
            Ternary => "RakuAST::Ternary",
            SemiList => "RakuAST::SemiList",
            PostcircumfixArrayIndex => "RakuAST::Postcircumfix::ArrayIndex",
            TermReduce => "RakuAST::Term::Reduce",
        }
    }

    /// raku's `Assignment` gist omits the empty `()` for the list form
    /// (`RakuAST::Assignment.new`), unlike the generic `.new()` (e.g. an empty
    /// `StatementList` still prints `RakuAST::StatementList.new()`).
    pub fn empty_parens_omitted(self) -> bool {
        matches!(self, RakuAstClass::Assignment)
    }

    pub fn constructor(self) -> Constructor {
        match self {
            RakuAstClass::Name => Constructor::FromIdentifier,
            _ => Constructor::New,
        }
    }

    /// Minimum width for aligning named `key => value` fields. raku's gist pads
    /// keys to the max length over the *shown* named fields of a node (computed
    /// per-instance in the renderer), but a few classes pad further to align
    /// with a declared-but-omitted attribute. `QuotedString` pads `segments`
    /// (8) to 10 to align with its unshown `processors`. This floor captures
    /// those exceptions; 0 = no floor (use the shown-field max directly).
    pub fn min_align_width(self) -> usize {
        match self {
            RakuAstClass::QuotedString => 10, // "processors" (unshown) > "segments"
            _ => 0,
        }
    }

    /// Extra `RakuAST::*` ancestor type names this node kind smartmatches beyond
    /// its own class, its `::`-namespace ancestors, and the universal
    /// `RakuAST::Node` — i.e. the *semantic* hierarchy (`RakuAST::Term` /
    /// `RakuAST::Expression`) whose names don't appear in the printed class name.
    /// Only classes verified against Rakudo are listed; an unlisted expression
    /// node is a documented gap (a missed match), never a false positive.
    pub fn semantic_ancestors(self) -> &'static [&'static str] {
        use RakuAstClass::*;
        // A Term is also an Expression.
        const TERM: &[&str] = &["RakuAST::Term", "RakuAST::Expression"];
        const EXPR: &[&str] = &["RakuAST::Expression"];
        match self {
            IntLiteral
            | RatLiteral
            | StrLiteral
            | QuotedString
            | VarLexical
            | TermReduce
            | Sub
            | Block
            | PointyBlock
            | CallName
            | CallNameWithoutParentheses => TERM,
            ApplyInfix | ApplyPrefix | ApplyPostfix | ApplyListInfix | Ternary => EXPR,
            _ => &[],
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

/// Construction (Phase 4): build a `Value::RakuAst` from a `RakuAST::*.new(...)`
/// / `.from-identifier(...)` call. Returns `Ok(None)` when the class/method is
/// not a supported constructor yet (so normal dispatch handles it). Covers the
/// single-positional-argument constructors: the literals (`.new`) and
/// `Name.from-identifier`.
pub fn construct(
    class_name: &str,
    method: &str,
    args: &[Value],
) -> Result<Option<Value>, RuntimeError> {
    let class = match (class_name, method) {
        ("RakuAST::IntLiteral", "new") => RakuAstClass::IntLiteral,
        ("RakuAST::RatLiteral", "new") => RakuAstClass::RatLiteral,
        ("RakuAST::StrLiteral", "new") => RakuAstClass::StrLiteral,
        ("RakuAST::Name", "from-identifier") => RakuAstClass::Name,
        _ => return Ok(None),
    };
    if args.len() != 1 {
        return Err(RuntimeError::new(format!(
            "{class_name}.{method} expects a single argument"
        )));
    }
    let node = RakuAstNode {
        class,
        fields: vec![RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(args[0].clone()),
        }],
    };
    Ok(Some(Value::rakuast(Box::new(node))))
}

/// A named-field / positional accessor on a RakuAST node (Phase 3). Returns the
/// field value as a mutsu `Value`, or `None` if `method` is not an accessor for
/// this node (so ordinary methods like `.gist` fall through). `.statements`
/// returns the positional children of a `StatementList`/`Blockoid` as a `List`.
pub fn node_accessor(node: &RakuAstNode, method: &str) -> Option<Value> {
    for f in &node.fields {
        if f.name == Some(method) {
            return Some(field_to_value(&f.value));
        }
    }
    if method == "statements" && matches!(node.class, RakuAstClass::StatementList) {
        let items = node
            .fields
            .iter()
            .map(|f| field_to_value(&f.value))
            .collect();
        return Some(Value::array(items));
    }
    // Positional-leaf accessors: a node whose single positional field is its
    // payload exposes it under a class-specific name (`IntLiteral.value`,
    // `Var::Lexical.name`). The named-field loop above runs first, so a class
    // with a *named* field of the same name (e.g. `Call::Name.name`) is unaffected.
    let positional_name = match node.class {
        RakuAstClass::IntLiteral | RakuAstClass::RatLiteral | RakuAstClass::StrLiteral => {
            Some("value")
        }
        RakuAstClass::VarLexical => Some("name"),
        _ => None,
    };
    if positional_name == Some(method)
        && let Some(f) = node.fields.first()
        && f.name.is_none()
    {
        return Some(field_to_value(&f.value));
    }
    None
}

fn field_to_value(fv: &RakuAstFieldValue) -> Value {
    match fv {
        RakuAstFieldValue::Node(v) => v.clone(),
        RakuAstFieldValue::List(items) => Value::array(items.clone()),
        // Colonpair adverbs (`:item`) are a rendering detail; expose as True.
        RakuAstFieldValue::Adverb(_) => Value::truth(true),
    }
}
