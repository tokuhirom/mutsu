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
mod formatter;
mod lower;
mod render;

pub use formatter::formatter_ast;
pub use lower::lower;

use crate::value::{RuntimeError, Value, ValueView};

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
    StatementModifierGiven,
    // Phase 2 slice 19: ternary.
    Ternary,
    // Phase 2 slice 22: positional subscripts.
    SemiList,
    PostcircumfixArrayIndex,
    // Phase 2 slice 25: reduction metaoperator.
    TermReduce,
    // Phase 2 slice 30: `True`/`False` (and other enum) literals.
    TermEnum,
    // Phase 2 slice 31: parenthesised expressions (`($x = 5)`).
    CircumfixParentheses,
    // Phase 2 slice 32: slurpy parameter markers (`*@a` / `**@a`).
    ParameterSlurpyFlattened,
    ParameterSlurpyUnflattened,
    // Phase 2 slice 33: array-composer literal (`[1, 2, 3]`).
    CircumfixArrayComposer,
    // Phase 2 slice 34: the `*` whatever term.
    TermWhatever,
    // Phase 2 slice 35: fat-arrow pairs (`a => 1`).
    FatArrow,
    // Phase 2 slice 36: the `do` statement prefix.
    StatementPrefixDo,
    // Phase 2 slice 37: the `try` statement prefix.
    StatementPrefixTry,
    // Phase 2 slice 38: the `gather` statement prefix.
    StatementPrefixGather,
    // Phase 2 slice 39: calling a term (`$f(…)`).
    CallTerm,
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
            StatementModifierGiven => "RakuAST::StatementModifier::Given",
            Ternary => "RakuAST::Ternary",
            SemiList => "RakuAST::SemiList",
            PostcircumfixArrayIndex => "RakuAST::Postcircumfix::ArrayIndex",
            TermReduce => "RakuAST::Term::Reduce",
            TermEnum => "RakuAST::Term::Enum",
            CircumfixParentheses => "RakuAST::Circumfix::Parentheses",
            ParameterSlurpyFlattened => "RakuAST::Parameter::Slurpy::Flattened",
            ParameterSlurpyUnflattened => "RakuAST::Parameter::Slurpy::Unflattened",
            CircumfixArrayComposer => "RakuAST::Circumfix::ArrayComposer",
            TermWhatever => "RakuAST::Term::Whatever",
            FatArrow => "RakuAST::FatArrow",
            StatementPrefixDo => "RakuAST::StatementPrefix::Do",
            StatementPrefixTry => "RakuAST::StatementPrefix::Try",
            StatementPrefixGather => "RakuAST::StatementPrefix::Gather",
            CallTerm => "RakuAST::Call::Term",
        }
    }

    /// raku's `Assignment` gist omits the empty `()` for the list form
    /// (`RakuAST::Assignment.new`), unlike the generic `.new()` (e.g. an empty
    /// `StatementList` still prints `RakuAST::StatementList.new()`).
    pub fn empty_parens_omitted(self) -> bool {
        matches!(self, RakuAstClass::Assignment | RakuAstClass::TermWhatever)
    }

    /// Whether the node renders as a bare class name with no constructor call at
    /// all (e.g. `RakuAST::Parameter::Slurpy::Flattened`), unlike the usual
    /// `Class.new(...)` / `Class.new` forms.
    pub fn renders_bare(self) -> bool {
        matches!(
            self,
            RakuAstClass::ParameterSlurpyFlattened | RakuAstClass::ParameterSlurpyUnflattened
        )
    }

    pub fn constructor(self) -> Constructor {
        match self {
            RakuAstClass::Name | RakuAstClass::TermEnum => Constructor::FromIdentifier,
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

/// Whether a registered RakuAST type object is a subtype of another RakuAST
/// type object. This mirrors [`Value::isa_check`] for node instances while also
/// covering abstract registry entries such as `RakuAST::Node` and
/// `RakuAST::Expression`.
pub fn type_object_isa(actual: &str, expected: &str) -> bool {
    if !is_registered_type_object(actual) || !is_registered_type_object(expected) {
        return false;
    }
    if actual == expected || expected == "RakuAST::Node" {
        return true;
    }
    if let Some(rest) = actual.strip_prefix(expected)
        && rest.starts_with("::")
    {
        return true;
    }
    match expected {
        "RakuAST::Expression" => {
            actual == "RakuAST::Term"
                || actual.starts_with("RakuAST::Term::")
                || semantic_type_object_ancestors(actual).contains(&expected)
        }
        "RakuAST::Term" => semantic_type_object_ancestors(actual).contains(&expected),
        _ => false,
    }
}

/// The model-layer MRO for a registered RakuAST type object. This intentionally
/// reflects mutsu's documented RakuAST hierarchy rather than pretending these
/// model types are ordinary entries in the runtime class registry.
pub fn type_object_mro(class_name: &str) -> Option<Vec<String>> {
    if !is_registered_type_object(class_name) {
        return None;
    }

    let mut mro = vec![class_name.to_string()];
    let mut namespace = class_name;
    while let Some((parent, _)) = namespace.rsplit_once("::") {
        if parent == "RakuAST" {
            break;
        }
        if is_registered_type_object(parent) && !mro.iter().any(|name| name == parent) {
            mro.push(parent.to_string());
        }
        namespace = parent;
    }
    for ancestor in semantic_type_object_ancestors(class_name) {
        if !mro.iter().any(|name| name == ancestor) {
            mro.push((*ancestor).to_string());
        }
    }
    if class_name == "RakuAST::Term" && !mro.iter().any(|name| name == "RakuAST::Expression") {
        mro.push("RakuAST::Expression".to_string());
    }
    if class_name != "RakuAST::Node" {
        mro.push("RakuAST::Node".to_string());
    }
    mro.push("Any".to_string());
    mro.push("Mu".to_string());
    Some(mro)
}

/// The immediate parent in mutsu's linearized RakuAST model hierarchy.
pub fn type_object_direct_parent(class_name: &str) -> Option<String> {
    type_object_mro(class_name)?.into_iter().nth(1)
}

fn semantic_type_object_ancestors(class_name: &str) -> &'static [&'static str] {
    const TERM: &[&str] = &["RakuAST::Term", "RakuAST::Expression"];
    const EXPR: &[&str] = &["RakuAST::Expression"];
    match class_name {
        "RakuAST::IntLiteral"
        | "RakuAST::RatLiteral"
        | "RakuAST::StrLiteral"
        | "RakuAST::QuotedString"
        | "RakuAST::Var::Lexical"
        | "RakuAST::Term::Reduce"
        | "RakuAST::Sub"
        | "RakuAST::Block"
        | "RakuAST::PointyBlock"
        | "RakuAST::Call::Name"
        | "RakuAST::Call::Name::WithoutParentheses" => TERM,
        "RakuAST::ApplyInfix"
        | "RakuAST::ApplyPrefix"
        | "RakuAST::ApplyPostfix"
        | "RakuAST::ApplyListInfix"
        | "RakuAST::Ternary" => EXPR,
        _ => &[],
    }
}

fn is_registered_type_object(class_name: &str) -> bool {
    if matches!(
        class_name,
        "RakuAST::Node"
            | "RakuAST::Expression"
            | "RakuAST::Term"
            | "RakuAST::Statement"
            | "RakuAST::Call"
            | "RakuAST::Var"
            | "RakuAST::VarDeclaration"
            | "RakuAST::Initializer"
            | "RakuAST::Type"
            | "RakuAST::Trait"
            | "RakuAST::ParameterTarget"
            | "RakuAST::Parameter::Slurpy"
            | "RakuAST::Postcircumfix"
            | "RakuAST::Circumfix"
            | "RakuAST::StatementModifier"
            | "RakuAST::StatementPrefix"
            | "RakuAST::MetaPostfix"
    ) {
        return true;
    }
    RAKUAST_CLASSES
        .iter()
        .any(|class| class.printed_name() == class_name)
}

const RAKUAST_CLASSES: &[RakuAstClass] = &[
    RakuAstClass::StatementList,
    RakuAstClass::StatementExpression,
    RakuAstClass::IntLiteral,
    RakuAstClass::RatLiteral,
    RakuAstClass::StrLiteral,
    RakuAstClass::QuotedString,
    RakuAstClass::CallName,
    RakuAstClass::CallNameWithoutParentheses,
    RakuAstClass::Name,
    RakuAstClass::ArgList,
    RakuAstClass::VarLexical,
    RakuAstClass::VarDeclarationSimple,
    RakuAstClass::InitializerAssign,
    RakuAstClass::ApplyInfix,
    RakuAstClass::Infix,
    RakuAstClass::ApplyPrefix,
    RakuAstClass::Prefix,
    RakuAstClass::ApplyPostfix,
    RakuAstClass::Postfix,
    RakuAstClass::Assignment,
    RakuAstClass::CallMethod,
    RakuAstClass::CallQuotedMethod,
    RakuAstClass::MetaPostfixHyper,
    RakuAstClass::Block,
    RakuAstClass::Blockoid,
    RakuAstClass::PointyBlock,
    RakuAstClass::Signature,
    RakuAstClass::Parameter,
    RakuAstClass::ParameterTargetVar,
    RakuAstClass::StatementIf,
    RakuAstClass::StatementLoopWhile,
    RakuAstClass::StatementLoop,
    RakuAstClass::StatementElsif,
    RakuAstClass::StatementFor,
    RakuAstClass::Sub,
    RakuAstClass::TypeSetting,
    RakuAstClass::StatementLoopRepeatWhile,
    RakuAstClass::ApplyListInfix,
    RakuAstClass::TypeSimple,
    RakuAstClass::TypeDefinedness,
    RakuAstClass::TraitWillBuild,
    RakuAstClass::TypeParameterized,
    RakuAstClass::TypeCoercion,
    RakuAstClass::Class,
    RakuAstClass::Method,
    RakuAstClass::Role,
    RakuAstClass::RoleBody,
    RakuAstClass::Label,
    RakuAstClass::StatementGiven,
    RakuAstClass::StatementWhen,
    RakuAstClass::StatementDefault,
    RakuAstClass::StatementModifierGiven,
    RakuAstClass::Ternary,
    RakuAstClass::SemiList,
    RakuAstClass::PostcircumfixArrayIndex,
    RakuAstClass::TermReduce,
    RakuAstClass::TermEnum,
    RakuAstClass::CircumfixParentheses,
    RakuAstClass::ParameterSlurpyFlattened,
    RakuAstClass::ParameterSlurpyUnflattened,
    RakuAstClass::CircumfixArrayComposer,
    RakuAstClass::TermWhatever,
    RakuAstClass::FatArrow,
    RakuAstClass::StatementPrefixDo,
    RakuAstClass::StatementPrefixTry,
    RakuAstClass::StatementPrefixGather,
    RakuAstClass::CallTerm,
];

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
    if class_name == "RakuAST::StatementList" && method == "new" {
        if !args.is_empty() {
            return Err(RuntimeError::new(
                "RakuAST::StatementList.new expects no arguments",
            ));
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::StatementList,
            fields: Vec::new(),
        }))));
    }
    if class_name == "RakuAST::Blockoid" && method == "new" {
        if args.len() != 1 {
            return Err(RuntimeError::new(
                "RakuAST::Blockoid.new expects a single StatementList argument",
            ));
        }
        require_rakuast_class(
            &args[0],
            RakuAstClass::StatementList,
            "RakuAST::Blockoid.new",
        )?;
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::Blockoid,
            fields: vec![RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(args[0].clone()),
            }],
        }))));
    }
    if class_name == "RakuAST::Sub" && method == "new" {
        let name = named_arg(args, "name");
        if let Some(value) = &name {
            require_rakuast_class(value, RakuAstClass::Name, "RakuAST::Sub.new")?;
        }
        let signature = named_arg(args, "signature");
        if let Some(value) = &signature {
            require_rakuast_class(value, RakuAstClass::Signature, "RakuAST::Sub.new")?;
        }
        let body = match named_arg(args, "body") {
            Some(value) => {
                require_rakuast_class(&value, RakuAstClass::Blockoid, "RakuAST::Sub.new")?;
                value
            }
            None => empty_blockoid(),
        };
        let mut fields = Vec::with_capacity(3);
        if let Some(name) = name {
            fields.push(RakuAstField {
                name: Some("name"),
                value: RakuAstFieldValue::Node(name),
            });
        }
        if let Some(signature) = signature {
            fields.push(RakuAstField {
                name: Some("signature"),
                value: RakuAstFieldValue::Node(signature),
            });
        }
        fields.push(RakuAstField {
            name: Some("body"),
            value: RakuAstFieldValue::Node(body),
        });
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::Sub,
            fields,
        }))));
    }
    if class_name == "RakuAST::Signature" && method == "new" {
        let parameters = named_arg(args, "parameters")
            .map(|value| {
                value.as_list_items().map(<[Value]>::to_vec).ok_or_else(|| {
                    RuntimeError::new("RakuAST::Signature.new expects `parameters` to be a list")
                })
            })
            .transpose()?
            .unwrap_or_default();
        for parameter in &parameters {
            require_rakuast_class(parameter, RakuAstClass::Parameter, "RakuAST::Signature.new")?;
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::Signature,
            fields: vec![RakuAstField {
                name: Some("parameters"),
                value: RakuAstFieldValue::List(parameters),
            }],
        }))));
    }
    if class_name == "RakuAST::Parameter" && method == "new" {
        let target = named_arg(args, "target").ok_or_else(|| {
            RuntimeError::new("RakuAST::Parameter.new requires a `target` argument")
        })?;
        require_rakuast_class(
            &target,
            RakuAstClass::ParameterTargetVar,
            "RakuAST::Parameter.new",
        )?;
        let mut fields = Vec::with_capacity(5);
        if let Some(type_node) = named_arg(args, "type") {
            require_rakuast_type(&type_node, "RakuAST::Parameter.new")?;
            fields.push(RakuAstField {
                name: Some("type"),
                value: RakuAstFieldValue::Node(type_node),
            });
        }
        if let Some(names) = named_arg(args, "names") {
            let names = names
                .as_list_items()
                .map(<[Value]>::to_vec)
                .ok_or_else(|| {
                    RuntimeError::new("RakuAST::Parameter.new expects `names` to be a list")
                })?;
            if names
                .iter()
                .any(|name| !matches!(name.view(), ValueView::Str(_)))
            {
                return Err(RuntimeError::new(
                    "RakuAST::Parameter.new expects `names` to contain strings",
                ));
            }
            fields.push(RakuAstField {
                name: Some("names"),
                value: RakuAstFieldValue::List(names),
            });
        }
        fields.push(RakuAstField {
            name: Some("target"),
            value: RakuAstFieldValue::Node(target),
        });
        if let Some(optional) = named_arg(args, "optional") {
            if !matches!(optional.view(), ValueView::Bool(_)) {
                return Err(RuntimeError::new(
                    "RakuAST::Parameter.new expects `optional` to be Bool",
                ));
            }
            fields.push(RakuAstField {
                name: Some("optional"),
                value: RakuAstFieldValue::Node(optional),
            });
        }
        if let Some(default) = named_arg(args, "default") {
            require_any_rakuast(&default, "RakuAST::Parameter.new", "default")?;
            fields.push(RakuAstField {
                name: Some("default"),
                value: RakuAstFieldValue::Node(default),
            });
        }
        if let Some(where_constraint) = named_arg(args, "where") {
            require_any_rakuast(&where_constraint, "RakuAST::Parameter.new", "where")?;
            fields.push(RakuAstField {
                name: Some("where"),
                value: RakuAstFieldValue::Node(where_constraint),
            });
        }
        if let Some(slurpy) = named_arg(args, "slurpy") {
            let slurpy = normalize_slurpy_marker(slurpy)?;
            fields.push(RakuAstField {
                name: Some("slurpy"),
                value: RakuAstFieldValue::Node(slurpy),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::Parameter,
            fields,
        }))));
    }
    if class_name == "RakuAST::VarDeclaration::Simple" && method == "new" {
        let sigil = named_arg(args, "sigil").ok_or_else(|| {
            RuntimeError::new("RakuAST::VarDeclaration::Simple.new requires a `sigil` argument")
        })?;
        let desigilname = named_arg(args, "desigilname").ok_or_else(|| {
            RuntimeError::new(
                "RakuAST::VarDeclaration::Simple.new requires a `desigilname` argument",
            )
        })?;
        require_rakuast_class(
            &desigilname,
            RakuAstClass::Name,
            "RakuAST::VarDeclaration::Simple.new",
        )?;
        let mut fields = vec![
            RakuAstField {
                name: Some("sigil"),
                value: RakuAstFieldValue::Node(sigil),
            },
            RakuAstField {
                name: Some("desigilname"),
                value: RakuAstFieldValue::Node(desigilname),
            },
        ];
        if let Some(initializer) = named_arg(args, "initializer") {
            require_rakuast_class(
                &initializer,
                RakuAstClass::InitializerAssign,
                "RakuAST::VarDeclaration::Simple.new",
            )?;
            fields.push(RakuAstField {
                name: Some("initializer"),
                value: RakuAstFieldValue::Node(initializer),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::VarDeclarationSimple,
            fields,
        }))));
    }
    // Single-positional-argument constructors: the literals, `Name.from-identifier`,
    // and the bare operator nodes (`Infix.new("+")`).
    if let Some(class) = single_positional_class(class_name, method) {
        if args.len() != 1 {
            return Err(RuntimeError::new(format!(
                "{class_name}.{method} expects a single argument"
            )));
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class,
            fields: vec![RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(args[0].clone()),
            }],
        }))));
    }
    // Multi-field named constructors: the named args map to same-named fields in
    // the class's schema order (`ApplyInfix.new(left => …, infix => …, right => …)`).
    if let Some((class, schema)) = multi_field_schema(class_name, method) {
        let mut fields = Vec::with_capacity(schema.len());
        for &fname in schema {
            let value = named_arg(args, fname).ok_or_else(|| {
                RuntimeError::new(format!(
                    "{class_name}.{method} requires a `{fname}` argument"
                ))
            })?;
            fields.push(RakuAstField {
                name: Some(fname),
                value: RakuAstFieldValue::Node(value),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class,
            fields,
        }))));
    }
    Ok(None)
}

fn empty_blockoid() -> Value {
    let statements = Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::StatementList,
        fields: Vec::new(),
    }));
    Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::Blockoid,
        fields: vec![RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(statements),
        }],
    }))
}

fn require_rakuast_class(
    value: &Value,
    expected: RakuAstClass,
    constructor: &str,
) -> Result<(), RuntimeError> {
    match value.view() {
        ValueView::RakuAst(node) if node.class == expected => Ok(()),
        _ => Err(RuntimeError::new(format!(
            "{constructor} expects a {} node",
            expected.printed_name()
        ))),
    }
}

fn require_any_rakuast(
    value: &Value,
    constructor: &str,
    argument: &str,
) -> Result<(), RuntimeError> {
    if matches!(value.view(), ValueView::RakuAst(_)) {
        Ok(())
    } else {
        Err(RuntimeError::new(format!(
            "{constructor} expects `{argument}` to be a RakuAST node"
        )))
    }
}

fn require_rakuast_type(value: &Value, constructor: &str) -> Result<(), RuntimeError> {
    match value.view() {
        ValueView::RakuAst(node)
            if matches!(
                node.class,
                RakuAstClass::TypeSimple
                    | RakuAstClass::TypeSetting
                    | RakuAstClass::TypeDefinedness
                    | RakuAstClass::TypeParameterized
                    | RakuAstClass::TypeCoercion
            ) =>
        {
            Ok(())
        }
        _ => Err(RuntimeError::new(format!(
            "{constructor} expects `type` to be a RakuAST type node"
        ))),
    }
}

fn normalize_slurpy_marker(value: Value) -> Result<Value, RuntimeError> {
    let class = match value.view() {
        ValueView::RakuAst(node)
            if matches!(
                node.class,
                RakuAstClass::ParameterSlurpyFlattened | RakuAstClass::ParameterSlurpyUnflattened
            ) =>
        {
            return Ok(value);
        }
        ValueView::Package(name) => match name.resolve().as_str() {
            "RakuAST::Parameter::Slurpy::Flattened" => RakuAstClass::ParameterSlurpyFlattened,
            "RakuAST::Parameter::Slurpy::Unflattened" => RakuAstClass::ParameterSlurpyUnflattened,
            _ => {
                return Err(RuntimeError::new(
                    "RakuAST::Parameter.new expects a RakuAST slurpy marker",
                ));
            }
        },
        _ => {
            return Err(RuntimeError::new(
                "RakuAST::Parameter.new expects a RakuAST slurpy marker",
            ));
        }
    };
    Ok(Value::rakuast(Box::new(RakuAstNode {
        class,
        fields: Vec::new(),
    })))
}

/// The class for a single-positional-argument constructor, or `None`.
fn single_positional_class(class_name: &str, method: &str) -> Option<RakuAstClass> {
    Some(match (class_name, method) {
        ("RakuAST::IntLiteral", "new") => RakuAstClass::IntLiteral,
        ("RakuAST::RatLiteral", "new") => RakuAstClass::RatLiteral,
        ("RakuAST::StrLiteral", "new") => RakuAstClass::StrLiteral,
        ("RakuAST::Name", "from-identifier") => RakuAstClass::Name,
        ("RakuAST::Term::Enum", "from-identifier") => RakuAstClass::TermEnum,
        ("RakuAST::Infix", "new") => RakuAstClass::Infix,
        ("RakuAST::Prefix", "new") => RakuAstClass::Prefix,
        ("RakuAST::Var::Lexical", "new") => RakuAstClass::VarLexical,
        ("RakuAST::Initializer::Assign", "new") => RakuAstClass::InitializerAssign,
        ("RakuAST::Type::Simple", "new") => RakuAstClass::TypeSimple,
        ("RakuAST::Type::Setting", "new") => RakuAstClass::TypeSetting,
        _ => return None,
    })
}

/// The class and ordered named-field schema for a multi-field constructor.
fn multi_field_schema(
    class_name: &str,
    method: &str,
) -> Option<(RakuAstClass, &'static [&'static str])> {
    Some(match (class_name, method) {
        ("RakuAST::Statement::Expression", "new") => {
            (RakuAstClass::StatementExpression, &["expression"][..])
        }
        ("RakuAST::ApplyInfix", "new") => {
            (RakuAstClass::ApplyInfix, &["left", "infix", "right"][..])
        }
        ("RakuAST::ApplyPrefix", "new") => (RakuAstClass::ApplyPrefix, &["prefix", "operand"][..]),
        ("RakuAST::ApplyPostfix", "new") => {
            (RakuAstClass::ApplyPostfix, &["operand", "postfix"][..])
        }
        ("RakuAST::Postfix", "new") => (RakuAstClass::Postfix, &["operator"][..]),
        ("RakuAST::Block", "new") => (RakuAstClass::Block, &["body"][..]),
        ("RakuAST::ParameterTarget::Var", "new") => {
            (RakuAstClass::ParameterTargetVar, &["name"][..])
        }
        _ => return None,
    })
}

/// Find a named (`key => value`) constructor argument, returning its value.
fn named_arg(args: &[Value], name: &str) -> Option<Value> {
    args.iter().find_map(|a| match a.view() {
        ValueView::Pair(k, v) => (k.as_str() == name).then(|| v.clone()),
        ValueView::ValuePair(k, v) => (k.to_string_value() == name).then(|| v.clone()),
        _ => None,
    })
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
        RakuAstClass::Blockoid => Some("statement-list"),
        RakuAstClass::InitializerAssign => Some("expression"),
        RakuAstClass::TypeSimple | RakuAstClass::TypeSetting => Some("name"),
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

/// Native methods currently exposed directly by a RakuAST model class.
///
/// This intentionally describes mutsu's implemented model API rather than
/// copying Rakudo's compiler-internal `IMPL-*` methods.  The result feeds
/// `.^methods(:local)`, so callers can discover constructors and accessors
/// without the RakuAST classes having ordinary registry entries.
pub fn local_method_names(class_name: &str) -> Option<Vec<&'static str>> {
    let class = class_from_name(class_name)?;
    let mut names = Vec::new();

    match class.constructor() {
        Constructor::FromIdentifier
            if matches!(class, RakuAstClass::Name | RakuAstClass::TermEnum) =>
        {
            names.push("from-identifier");
        }
        Constructor::New if constructor_is_supported(class) => names.push("new"),
        _ => {}
    }

    names.extend(accessor_names(class));
    if class == RakuAstClass::StatementList {
        names.push("add-statement");
    }
    names.sort_unstable();
    names.dedup();
    Some(names)
}

/// Model fields declared directly by a RakuAST class, for `.^attributes(:local)`.
///
/// As with [`local_method_names`], these are mutsu's public model fields rather
/// than Rakudo's backend storage slots.
pub fn local_attribute_names(class_name: &str) -> Option<&'static [&'static str]> {
    class_from_name(class_name).map(accessor_names)
}

fn class_from_name(class_name: &str) -> Option<RakuAstClass> {
    RAKUAST_CLASSES
        .iter()
        .copied()
        .find(|class| class.printed_name() == class_name)
}

fn constructor_is_supported(class: RakuAstClass) -> bool {
    matches!(
        class,
        RakuAstClass::StatementList
            | RakuAstClass::IntLiteral
            | RakuAstClass::RatLiteral
            | RakuAstClass::StrLiteral
            | RakuAstClass::Infix
            | RakuAstClass::Prefix
            | RakuAstClass::VarLexical
            | RakuAstClass::StatementExpression
            | RakuAstClass::ApplyInfix
            | RakuAstClass::ApplyPrefix
            | RakuAstClass::ApplyPostfix
            | RakuAstClass::Postfix
            | RakuAstClass::Block
            | RakuAstClass::Blockoid
            | RakuAstClass::Sub
            | RakuAstClass::Signature
            | RakuAstClass::Parameter
            | RakuAstClass::ParameterTargetVar
            | RakuAstClass::VarDeclarationSimple
            | RakuAstClass::InitializerAssign
            | RakuAstClass::TypeSimple
            | RakuAstClass::TypeSetting
    )
}

fn accessor_names(class: RakuAstClass) -> &'static [&'static str] {
    use RakuAstClass::*;
    match class {
        StatementList => &["statements"],
        StatementExpression => &["expression", "loop-modifier"],
        IntLiteral | RatLiteral | StrLiteral => &["value"],
        VarLexical => &["name"],
        ApplyInfix => &["left", "infix", "right"],
        ApplyPrefix => &["prefix", "operand"],
        ApplyPostfix => &["operand", "postfix"],
        Postfix => &["operator"],
        Block => &["body"],
        Blockoid => &["statement-list"],
        Sub => &["name", "signature", "body"],
        Signature => &["parameters"],
        Parameter => &[
            "type", "names", "target", "optional", "default", "where", "slurpy",
        ],
        ParameterTargetVar => &["name"],
        VarDeclarationSimple => &["sigil", "desigilname", "initializer"],
        InitializerAssign => &["expression"],
        TypeSimple | TypeSetting => &["name"],
        _ => &[],
    }
}

fn field_to_value(fv: &RakuAstFieldValue) -> Value {
    match fv {
        RakuAstFieldValue::Node(v) => v.clone(),
        RakuAstFieldValue::List(items) => Value::array(items.clone()),
        // Colonpair adverbs (`:item`) are a rendering detail; expose as True.
        RakuAstFieldValue::Adverb(_) => Value::truth(true),
    }
}
