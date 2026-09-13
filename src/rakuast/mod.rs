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
mod fields;
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
    QuotedRegex,
    RegexSequence,
    RegexLiteral,
    RegexQuote,
    RegexWithWhitespace,
    RegexGroup,
    RegexCapturingGroup,
    RegexNamedCapture,
    RegexInterpolation,
    RegexAlternation,
    RegexQuantifiedAtom,
    RegexQuantifierZeroOrMore,
    RegexQuantifierOneOrMore,
    RegexQuantifierZeroOrOne,
    RegexCharClassDigit,
    ColonPairTrue,
    RegexDeclaration,
    TokenDeclaration,
    RuleDeclaration,
    Grammar,
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
    FunctionInfix,
    ApplyPrefix,
    Prefix,
    ApplyPostfix,
    Postfix,
    Assignment,
    MetaInfixAssign,
    CallMethod,
    // Phase 2 slice 23: quoted method names.
    CallQuotedMethod,
    // Phase 2 slice 26: hyper method calls.
    MetaPostfixHyper,
    // Hyper infix operators (`@a >>+<< @b`).
    MetaInfixHyper,
    // Phase 2 slice 3: blocks & pointy blocks.
    Block,
    Blockoid,
    PointyBlock,
    Signature,
    Parameter,
    ParameterTargetVar,
    // Phase 2 slice 4: conditionals and loops.
    StatementIf,
    StatementUnless,
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
    // `enum Color <Red Green>` -> `Type::Enum(name, term)`.
    TypeEnum,
    // Phase 2 slice 20: definite types (`Int:D` / `Int:U`).
    TypeDefinedness,
    // Phase 2 slice 27: attribute build-time defaults.
    TraitWillBuild,
    // Routine return types written as a trait (`sub f() returns Int` / `of Int`).
    TraitReturns,
    TraitOf,
    // Phase 2 slice 21: parameterised types (`Array[Int]`).
    TypeParameterized,
    // Phase 2 slice 29: coercion types (`Int()`).
    TypeCoercion,
    // Type parameters such as `::T $value`.
    TypeCapture,
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
    StatementModifierIf,
    StatementModifierUnless,
    StatementModifierWith,
    StatementModifierWithout,
    // The `with`/`without`/`orwith` BLOCK forms.
    StatementWith,
    StatementWithout,
    StatementOrwith,
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
    // ADR-0033 Phase 2: a `*` that participates in Whatever-priming
    // (`* + 1`'s left operand) vs a bare Whatever value.
    WhateverCodeArgument,
    // ADR-0033 Phase 2: the `**` hyper-whatever term (read direction only —
    // `**` is out of scope for priming, see the ADR's §1).
    TermHyperWhatever,
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
    // Phasers (`BEGIN`, `INIT`, `LEAVE`, ...). raku models each kind as its own
    // `RakuAST::StatementPrefix::Phaser::<Kind>` class wrapping a positional
    // `Block`; mutsu's one `Stmt::Phaser { kind, .. }` maps onto them 1:1.
    // `constant X = 5` — a declaration of its own, not a scoped `my`.
    VarDeclarationConstant,
    // A bareword naming something the unit declared that is not a type — a
    // `constant`, in practice.
    TermName,
    // `.^name` — a metamethod call, distinct from `.?`/`.+`/`.*` dispatch.
    CallMetaMethod,
    // `until` / `repeat … until` — raku's own classes, not a negated `while`.
    StatementLoopUntil,
    StatementLoopRepeatUntil,
    // Class/role declaration traits (`is Parent`, `does Role`, `is rw`).
    TraitIs,
    TraitDoes,
    StatementPrefixPhaserBegin,
    StatementPrefixPhaserCheck,
    StatementPrefixPhaserInit,
    StatementPrefixPhaserEnd,
    StatementPrefixPhaserEnter,
    StatementPrefixPhaserLeave,
    StatementPrefixPhaserKeep,
    StatementPrefixPhaserUndo,
    StatementPrefixPhaserFirst,
    StatementPrefixPhaserNext,
    StatementPrefixPhaserLast,
    StatementPrefixPhaserPre,
    StatementPrefixPhaserPost,
    StatementPrefixPhaserQuit,
    StatementPrefixPhaserClose,
    // `CATCH { ... }` — its own statement class, not a block phaser. Its body is
    // a topic block that additionally sets `exception => 1`.
    StatementCatch,
    // `subset S of T where P` — raku files it under `RakuAST::Type::`, not
    // under the declaration classes.
    TypeSubset,
    // `module M { }` / `package P { }` — siblings of `RakuAST::Class`, one class
    // per declarator keyword rather than a shared node with a `kind` field.
    Module,
    Package,
    // `submethod m { }` — a sibling of `RakuAST::Method`, again keyword-per-class.
    Submethod,
    // `self` — a term with no fields of its own.
    TermSelf,
    // An argument-less core `use` pragma (`use strict`, `use fatal`, ...).
    Pragma,
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
            QuotedRegex => "RakuAST::QuotedRegex",
            RegexSequence => "RakuAST::Regex::Sequence",
            RegexLiteral => "RakuAST::Regex::Literal",
            RegexQuote => "RakuAST::Regex::Quote",
            RegexWithWhitespace => "RakuAST::Regex::WithWhitespace",
            RegexGroup => "RakuAST::Regex::Group",
            RegexCapturingGroup => "RakuAST::Regex::CapturingGroup",
            RegexNamedCapture => "RakuAST::Regex::NamedCapture",
            RegexInterpolation => "RakuAST::Regex::Interpolation",
            RegexAlternation => "RakuAST::Regex::Alternation",
            RegexQuantifiedAtom => "RakuAST::Regex::QuantifiedAtom",
            RegexQuantifierZeroOrMore => "RakuAST::Regex::Quantifier::ZeroOrMore",
            RegexQuantifierOneOrMore => "RakuAST::Regex::Quantifier::OneOrMore",
            RegexQuantifierZeroOrOne => "RakuAST::Regex::Quantifier::ZeroOrOne",
            RegexCharClassDigit => "RakuAST::Regex::CharClass::Digit",
            ColonPairTrue => "RakuAST::ColonPair::True",
            RegexDeclaration => "RakuAST::RegexDeclaration",
            TokenDeclaration => "RakuAST::TokenDeclaration",
            RuleDeclaration => "RakuAST::RuleDeclaration",
            Grammar => "RakuAST::Grammar",
            CallName => "RakuAST::Call::Name",
            CallNameWithoutParentheses => "RakuAST::Call::Name::WithoutParentheses",
            Name => "RakuAST::Name",
            ArgList => "RakuAST::ArgList",
            VarLexical => "RakuAST::Var::Lexical",
            VarDeclarationSimple => "RakuAST::VarDeclaration::Simple",
            InitializerAssign => "RakuAST::Initializer::Assign",
            ApplyInfix => "RakuAST::ApplyInfix",
            Infix => "RakuAST::Infix",
            FunctionInfix => "RakuAST::FunctionInfix",
            ApplyPrefix => "RakuAST::ApplyPrefix",
            Prefix => "RakuAST::Prefix",
            ApplyPostfix => "RakuAST::ApplyPostfix",
            Postfix => "RakuAST::Postfix",
            Assignment => "RakuAST::Assignment",
            MetaInfixAssign => "RakuAST::MetaInfix::Assign",
            CallMethod => "RakuAST::Call::Method",
            CallQuotedMethod => "RakuAST::Call::QuotedMethod",
            MetaPostfixHyper => "RakuAST::MetaPostfix::Hyper",
            MetaInfixHyper => "RakuAST::MetaInfix::Hyper",
            Block => "RakuAST::Block",
            Blockoid => "RakuAST::Blockoid",
            PointyBlock => "RakuAST::PointyBlock",
            Signature => "RakuAST::Signature",
            Parameter => "RakuAST::Parameter",
            ParameterTargetVar => "RakuAST::ParameterTarget::Var",
            StatementIf => "RakuAST::Statement::If",
            StatementUnless => "RakuAST::Statement::Unless",
            StatementLoopWhile => "RakuAST::Statement::Loop::While",
            StatementLoop => "RakuAST::Statement::Loop",
            StatementElsif => "RakuAST::Statement::Elsif",
            StatementFor => "RakuAST::Statement::For",
            Sub => "RakuAST::Sub",
            TypeSetting => "RakuAST::Type::Setting",
            StatementLoopRepeatWhile => "RakuAST::Statement::Loop::RepeatWhile",
            ApplyListInfix => "RakuAST::ApplyListInfix",
            TypeSimple => "RakuAST::Type::Simple",
            TypeEnum => "RakuAST::Type::Enum",
            TypeDefinedness => "RakuAST::Type::Definedness",
            TraitWillBuild => "RakuAST::Trait::WillBuild",
            TraitReturns => "RakuAST::Trait::Returns",
            TraitOf => "RakuAST::Trait::Of",
            TypeParameterized => "RakuAST::Type::Parameterized",
            TypeCoercion => "RakuAST::Type::Coercion",
            TypeCapture => "RakuAST::Type::Capture",
            Class => "RakuAST::Class",
            Method => "RakuAST::Method",
            Role => "RakuAST::Role",
            RoleBody => "RakuAST::RoleBody",
            Label => "RakuAST::Label",
            StatementGiven => "RakuAST::Statement::Given",
            StatementWhen => "RakuAST::Statement::When",
            StatementDefault => "RakuAST::Statement::Default",
            StatementModifierGiven => "RakuAST::StatementModifier::Given",
            StatementModifierIf => "RakuAST::StatementModifier::If",
            StatementModifierUnless => "RakuAST::StatementModifier::Unless",
            StatementModifierWith => "RakuAST::StatementModifier::With",
            StatementModifierWithout => "RakuAST::StatementModifier::Without",
            StatementWith => "RakuAST::Statement::With",
            StatementWithout => "RakuAST::Statement::Without",
            StatementOrwith => "RakuAST::Statement::Orwith",
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
            WhateverCodeArgument => "RakuAST::WhateverCode::Argument",
            TermHyperWhatever => "RakuAST::Term::HyperWhatever",
            FatArrow => "RakuAST::FatArrow",
            StatementPrefixDo => "RakuAST::StatementPrefix::Do",
            StatementPrefixTry => "RakuAST::StatementPrefix::Try",
            StatementPrefixGather => "RakuAST::StatementPrefix::Gather",
            CallTerm => "RakuAST::Call::Term",
            VarDeclarationConstant => "RakuAST::VarDeclaration::Constant",
            TermName => "RakuAST::Term::Name",
            CallMetaMethod => "RakuAST::Call::MetaMethod",
            StatementLoopUntil => "RakuAST::Statement::Loop::Until",
            StatementLoopRepeatUntil => "RakuAST::Statement::Loop::RepeatUntil",
            TraitIs => "RakuAST::Trait::Is",
            TraitDoes => "RakuAST::Trait::Does",
            StatementPrefixPhaserBegin => "RakuAST::StatementPrefix::Phaser::Begin",
            StatementPrefixPhaserCheck => "RakuAST::StatementPrefix::Phaser::Check",
            StatementPrefixPhaserInit => "RakuAST::StatementPrefix::Phaser::Init",
            StatementPrefixPhaserEnd => "RakuAST::StatementPrefix::Phaser::End",
            StatementPrefixPhaserEnter => "RakuAST::StatementPrefix::Phaser::Enter",
            StatementPrefixPhaserLeave => "RakuAST::StatementPrefix::Phaser::Leave",
            StatementPrefixPhaserKeep => "RakuAST::StatementPrefix::Phaser::Keep",
            StatementPrefixPhaserUndo => "RakuAST::StatementPrefix::Phaser::Undo",
            StatementPrefixPhaserFirst => "RakuAST::StatementPrefix::Phaser::First",
            StatementPrefixPhaserNext => "RakuAST::StatementPrefix::Phaser::Next",
            StatementPrefixPhaserLast => "RakuAST::StatementPrefix::Phaser::Last",
            StatementPrefixPhaserPre => "RakuAST::StatementPrefix::Phaser::Pre",
            StatementPrefixPhaserPost => "RakuAST::StatementPrefix::Phaser::Post",
            StatementPrefixPhaserQuit => "RakuAST::StatementPrefix::Phaser::Quit",
            StatementPrefixPhaserClose => "RakuAST::StatementPrefix::Phaser::Close",
            StatementCatch => "RakuAST::Statement::Catch",
            TypeSubset => "RakuAST::Type::Subset",
            Module => "RakuAST::Module",
            Package => "RakuAST::Package",
            Submethod => "RakuAST::Submethod",
            TermSelf => "RakuAST::Term::Self",
            Pragma => "RakuAST::Pragma",
        }
    }

    /// raku's `Assignment` gist omits the empty `()` for the list form
    /// (`RakuAST::Assignment.new`), unlike the generic `.new()` (e.g. an empty
    /// `StatementList` still prints `RakuAST::StatementList.new()`).
    pub fn empty_parens_omitted(self) -> bool {
        matches!(
            self,
            RakuAstClass::Assignment
                | RakuAstClass::TermWhatever
                | RakuAstClass::WhateverCodeArgument
                | RakuAstClass::TermHyperWhatever
                | RakuAstClass::TermSelf
                | RakuAstClass::RegexQuantifierZeroOrMore
                | RakuAstClass::RegexQuantifierOneOrMore
                | RakuAstClass::RegexQuantifierZeroOrOne
                | RakuAstClass::RegexCharClassDigit
        )
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
            | QuotedRegex
            | TypeEnum
            | VarLexical
            | TermReduce
            | Sub
            | Block
            | PointyBlock
            | CallName
            | CallNameWithoutParentheses
            | RegexDeclaration
            | TokenDeclaration
            | RuleDeclaration
            // `RakuAST::WhateverCode::Argument` does not start with
            // `RakuAST::Term::`, so it does not get Term/Expression for free
            // from the name-prefix rule in `type_object_isa` — it must be
            // listed explicitly (ADR-0033 Phase 2 §2.4). Measured MRO:
            // `Argument, Term, Termish, Expression, ..., Node`.
            | WhateverCodeArgument
            // Measured MRO: `Self, Term, Termish, Expression, ..., Node`. The
            // `RakuAST::Term::` name prefix already answers `~~ RakuAST::Term`
            // for an instance, but `RakuAST::Expression` is only reachable
            // through this list.
            | TermSelf => TERM,
            ApplyInfix | ApplyPrefix | ApplyPostfix | ApplyListInfix | Ternary => EXPR,
            RegexLiteral
            | RegexQuote
            | RegexGroup
            | RegexCapturingGroup
            | RegexNamedCapture
            | RegexInterpolation
            | RegexWithWhitespace => &[
                "RakuAST::Regex::Atom",
                "RakuAST::Regex::Term",
                "RakuAST::Regex",
            ],
            RegexSequence | RegexAlternation => &["RakuAST::Regex"],
            RegexQuantifiedAtom => &["RakuAST::Regex::Term", "RakuAST::Regex"],
            RegexCharClassDigit => &[
                "RakuAST::Regex::CharClass",
                "RakuAST::Regex::Atom",
                "RakuAST::Regex::Term",
                "RakuAST::Regex",
            ],
            Grammar => &[
                "RakuAST::Class",
                "RakuAST::Package",
                "RakuAST::Term",
                "RakuAST::Expression",
            ],
            ColonPairTrue => &["RakuAST::Term", "RakuAST::Expression"],
            Pragma => &["RakuAST::Statement"],
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
    if semantic_type_object_ancestors(actual).contains(&expected) {
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
        | "RakuAST::Type::Enum"
        | "RakuAST::Var::Lexical"
        | "RakuAST::Term::Reduce"
        | "RakuAST::Sub"
        | "RakuAST::Block"
        | "RakuAST::PointyBlock"
        | "RakuAST::Call::Name"
        | "RakuAST::Call::Name::WithoutParentheses"
        | "RakuAST::QuotedRegex"
        // Same "not RakuAST::Term::"-prefixed" gap as the instance-level
        // `semantic_ancestors` above.
        | "RakuAST::WhateverCode::Argument" => TERM,
        "RakuAST::ApplyInfix"
        | "RakuAST::ApplyPrefix"
        | "RakuAST::ApplyPostfix"
        | "RakuAST::ApplyListInfix"
        | "RakuAST::Ternary" => EXPR,
        "RakuAST::Grammar" => &[
            "RakuAST::Class",
            "RakuAST::Package",
            "RakuAST::Term",
            "RakuAST::Expression",
        ],
        "RakuAST::Regex::Literal"
        | "RakuAST::Regex::Quote"
        | "RakuAST::Regex::Group"
        | "RakuAST::Regex::CapturingGroup"
        | "RakuAST::Regex::NamedCapture"
        | "RakuAST::Regex::Interpolation"
        | "RakuAST::Regex::WithWhitespace" => &[
            "RakuAST::Regex::Atom",
            "RakuAST::Regex::Term",
            "RakuAST::Regex",
        ],
        "RakuAST::Regex::Sequence" | "RakuAST::Regex::Alternation" => {
            &["RakuAST::Regex"]
        },
        "RakuAST::Regex::QuantifiedAtom" =>
            &["RakuAST::Regex::Term", "RakuAST::Regex"],
        "RakuAST::Regex::CharClass::Digit" => &[
            "RakuAST::Regex::CharClass",
            "RakuAST::Regex::Atom",
            "RakuAST::Regex::Term",
            "RakuAST::Regex",
        ],
        "RakuAST::ColonPair::True" => &["RakuAST::Term", "RakuAST::Expression"],
        "RakuAST::Pragma" => &["RakuAST::Statement"],
        "RakuAST::RegexDeclaration"
        | "RakuAST::TokenDeclaration"
        | "RakuAST::RuleDeclaration" => &[
            "RakuAST::Term",
            "RakuAST::Expression",
        ],
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
            // The two abstract halves of the StatementModifier hierarchy: an
            // absent `condition-modifier` / `loop-modifier` answers with one of
            // these type objects, so they have to be nameable.
            | "RakuAST::StatementModifier::Condition"
            | "RakuAST::StatementModifier::Loop"
            | "RakuAST::StatementPrefix"
            | "RakuAST::MetaPostfix"
            | "RakuAST::MetaInfix"
            | "RakuAST::Regex"
            | "RakuAST::Regex::Atom"
            | "RakuAST::Regex::Term"
            | "RakuAST::Regex::Quantifier"
            | "RakuAST::Regex::CharClass"
            | "RakuAST::ColonPair"
            | "RakuAST::QuotePair"
            | "RakuAST::Package"
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
    RakuAstClass::QuotedRegex,
    RakuAstClass::RegexSequence,
    RakuAstClass::RegexLiteral,
    RakuAstClass::RegexQuote,
    RakuAstClass::RegexWithWhitespace,
    RakuAstClass::RegexGroup,
    RakuAstClass::RegexCapturingGroup,
    RakuAstClass::RegexNamedCapture,
    RakuAstClass::RegexInterpolation,
    RakuAstClass::RegexAlternation,
    RakuAstClass::RegexQuantifiedAtom,
    RakuAstClass::RegexQuantifierZeroOrMore,
    RakuAstClass::RegexQuantifierOneOrMore,
    RakuAstClass::RegexQuantifierZeroOrOne,
    RakuAstClass::RegexCharClassDigit,
    RakuAstClass::ColonPairTrue,
    RakuAstClass::RegexDeclaration,
    RakuAstClass::TokenDeclaration,
    RakuAstClass::RuleDeclaration,
    RakuAstClass::Grammar,
    RakuAstClass::CallName,
    RakuAstClass::CallNameWithoutParentheses,
    RakuAstClass::Name,
    RakuAstClass::ArgList,
    RakuAstClass::VarLexical,
    RakuAstClass::VarDeclarationSimple,
    RakuAstClass::InitializerAssign,
    RakuAstClass::ApplyInfix,
    RakuAstClass::Infix,
    RakuAstClass::FunctionInfix,
    RakuAstClass::ApplyPrefix,
    RakuAstClass::Prefix,
    RakuAstClass::ApplyPostfix,
    RakuAstClass::Postfix,
    RakuAstClass::Assignment,
    RakuAstClass::MetaInfixAssign,
    RakuAstClass::CallMethod,
    RakuAstClass::CallQuotedMethod,
    RakuAstClass::MetaPostfixHyper,
    RakuAstClass::MetaInfixHyper,
    RakuAstClass::Block,
    RakuAstClass::Blockoid,
    RakuAstClass::PointyBlock,
    RakuAstClass::Signature,
    RakuAstClass::Parameter,
    RakuAstClass::ParameterTargetVar,
    RakuAstClass::StatementIf,
    RakuAstClass::StatementUnless,
    RakuAstClass::StatementLoopWhile,
    RakuAstClass::StatementLoop,
    RakuAstClass::StatementElsif,
    RakuAstClass::StatementFor,
    RakuAstClass::Sub,
    RakuAstClass::TypeSetting,
    RakuAstClass::StatementLoopRepeatWhile,
    RakuAstClass::ApplyListInfix,
    RakuAstClass::TypeSimple,
    RakuAstClass::TypeEnum,
    RakuAstClass::TypeDefinedness,
    RakuAstClass::TraitWillBuild,
    RakuAstClass::TraitReturns,
    RakuAstClass::TraitOf,
    RakuAstClass::TypeParameterized,
    RakuAstClass::TypeCoercion,
    RakuAstClass::TypeCapture,
    RakuAstClass::Class,
    RakuAstClass::Method,
    RakuAstClass::Role,
    RakuAstClass::RoleBody,
    RakuAstClass::Label,
    RakuAstClass::StatementGiven,
    RakuAstClass::StatementWhen,
    RakuAstClass::StatementDefault,
    RakuAstClass::StatementModifierGiven,
    RakuAstClass::StatementModifierIf,
    RakuAstClass::StatementModifierUnless,
    RakuAstClass::StatementModifierWith,
    RakuAstClass::StatementModifierWithout,
    RakuAstClass::StatementWith,
    RakuAstClass::StatementWithout,
    RakuAstClass::StatementOrwith,
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
    RakuAstClass::WhateverCodeArgument,
    RakuAstClass::TermHyperWhatever,
    RakuAstClass::FatArrow,
    RakuAstClass::StatementPrefixDo,
    RakuAstClass::StatementPrefixTry,
    RakuAstClass::StatementPrefixGather,
    RakuAstClass::CallTerm,
    RakuAstClass::VarDeclarationConstant,
    RakuAstClass::TermName,
    RakuAstClass::CallMetaMethod,
    RakuAstClass::StatementLoopUntil,
    RakuAstClass::StatementLoopRepeatUntil,
    RakuAstClass::TraitIs,
    RakuAstClass::TraitDoes,
    RakuAstClass::StatementPrefixPhaserBegin,
    RakuAstClass::StatementPrefixPhaserCheck,
    RakuAstClass::StatementPrefixPhaserInit,
    RakuAstClass::StatementPrefixPhaserEnd,
    RakuAstClass::StatementPrefixPhaserEnter,
    RakuAstClass::StatementPrefixPhaserLeave,
    RakuAstClass::StatementPrefixPhaserKeep,
    RakuAstClass::StatementPrefixPhaserUndo,
    RakuAstClass::StatementPrefixPhaserFirst,
    RakuAstClass::StatementPrefixPhaserNext,
    RakuAstClass::StatementPrefixPhaserLast,
    RakuAstClass::StatementPrefixPhaserPre,
    RakuAstClass::StatementPrefixPhaserPost,
    RakuAstClass::StatementPrefixPhaserQuit,
    RakuAstClass::StatementPrefixPhaserClose,
    RakuAstClass::StatementCatch,
    RakuAstClass::TypeSubset,
    RakuAstClass::Module,
    RakuAstClass::Package,
    RakuAstClass::Submethod,
    RakuAstClass::TermSelf,
    RakuAstClass::Pragma,
];

/// Entry point for `Str.AST`: parse the source, convert, wrap in `Value::RakuAst`.
pub fn str_dot_ast(source: &str) -> Result<Value, RuntimeError> {
    let (stmts, _finish) = crate::parse_dispatch::parse_source(source)?;
    let node = convert::statement_list(&stmts)?;
    Ok(Value::rakuast(Box::new(node)))
}

/// Entry point for `Str.AST($slang)`: parse the source under the localized
/// surface syntax of the `L10N::<$slang>` distribution.
///
/// Rakudo implements the argument by `use`ing `L10N::<$slang>` for the duration
/// of the sub-parse, which mixes that distribution's role into the MAIN slang
/// grammar. mutsu reuses the ADR-0026 activation machinery for the same effect:
/// the module is loaded in the activation sub-interpreter, its
/// `$*LANG.define_slang` registration hands back the role's token/mapping
/// overrides, and those become this parse's L10N vocabulary. The vocabulary
/// is preseeded rather than merely set, because `parse_source` resets the
/// unit's parser state on the way in.
///
/// An absent or undefined `$slang` (rakudo's `Mu $slang?` default) is the
/// plain parse. Any other value names the module verbatim — rakudo has no
/// special case for `"Raku"` either, and `.AST("Raku")` looks for `L10N::Raku`.
pub fn str_dot_ast_with_slang(source: &str, slang: Option<&str>) -> Result<Value, RuntimeError> {
    let Some(slang) = slang.filter(|s| !s.is_empty()) else {
        return str_dot_ast(source);
    };
    let module = format!("L10N::{slang}");
    let activation = crate::runtime::slang_activation::run_slang_activation(
        module.clone(),
        crate::parser::parser_lib_paths_for_slang(),
    )
    .map_err(|e| RuntimeError::new(format!("Could not find {module}: {e}")))?;

    let saved_modes = crate::parser::slang_modes();
    let saved_vocabulary = crate::parser::l10n_vocabulary_for_restore();
    let result = (|| {
        crate::parser::apply_slang_overrides(&activation.rules).map_err(RuntimeError::new)?;
        crate::parser::set_l10n_preseed(crate::parser::l10n_vocabulary_for_restore());
        str_dot_ast(source)
    })();
    crate::parser::set_l10n_preseed(None);
    crate::parser::restore_slang_state(saved_modes, saved_vocabulary);
    result
}

/// `.gist` / `.raku` / `.Str` of a RakuAST node.
pub fn node_gist(node: &RakuAstNode) -> String {
    render::render_node(node, 0)
}

/// Construction (Phase 4): build a `Value::RakuAst` from a `RakuAST::*.new(...)`
/// / `.from-identifier(...)` call. Returns `Ok(None)` when the class/method is
/// not a supported constructor yet (so normal dispatch handles it). Covers the
/// single-positional-argument constructors such as literals, names, and
/// return-type traits, plus the supported named-field constructors.
pub fn construct(
    class_name: &str,
    method: &str,
    args: &[Value],
) -> Result<Option<Value>, RuntimeError> {
    if class_name == "RakuAST::QuotedString" && method == "new" {
        let segments = named_arg(args, "segments")
            .ok_or_else(|| RuntimeError::new("RakuAST::QuotedString.new requires `segments`"))?
            .as_list_items()
            .map(<[Value]>::to_vec)
            .ok_or_else(|| {
                RuntimeError::new("RakuAST::QuotedString.new expects `segments` to be a list")
            })?;
        for segment in &segments {
            require_any_rakuast(segment, "RakuAST::QuotedString.new", "segments")?;
        }
        let processors = named_arg(args, "processors")
            .map(|value| {
                value.as_list_items().map(<[Value]>::to_vec).ok_or_else(|| {
                    RuntimeError::new("RakuAST::QuotedString.new expects `processors` to be a list")
                })
            })
            .transpose()?;
        if let Some(processors) = &processors
            && processors
                .iter()
                .any(|processor| !matches!(processor.view(), ValueView::Str(_)))
        {
            return Err(RuntimeError::new(
                "RakuAST::QuotedString.new expects `processors` to contain strings",
            ));
        }
        let mut fields = Vec::with_capacity(2);
        if let Some(processors) = processors {
            fields.push(RakuAstField {
                name: Some("processors"),
                value: RakuAstFieldValue::List(processors),
            });
        }
        fields.push(RakuAstField {
            name: Some("segments"),
            value: RakuAstFieldValue::List(segments),
        });
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::QuotedString,
            fields,
        }))));
    }
    if class_name == "RakuAST::Type::Enum" && method == "new" {
        let name = named_arg(args, "name")
            .ok_or_else(|| RuntimeError::new("RakuAST::Type::Enum.new requires `name`"))?;
        require_rakuast_class(&name, RakuAstClass::Name, "RakuAST::Type::Enum.new")?;
        let term = named_arg(args, "term")
            .ok_or_else(|| RuntimeError::new("RakuAST::Type::Enum.new requires `term`"))?;
        require_rakuast_class(&term, RakuAstClass::QuotedString, "RakuAST::Type::Enum.new")
            .or_else(|_| {
                require_rakuast_class(
                    &term,
                    RakuAstClass::CircumfixParentheses,
                    "RakuAST::Type::Enum.new",
                )
            })?;
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TypeEnum,
            fields: vec![
                RakuAstField {
                    name: Some("name"),
                    value: RakuAstFieldValue::Node(name),
                },
                RakuAstField {
                    name: Some("term"),
                    value: RakuAstFieldValue::Node(term),
                },
            ],
        }))));
    }
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
    if class_name == "RakuAST::Pragma" && method == "new" {
        let name = named_arg(args, "name")
            .ok_or_else(|| RuntimeError::new("RakuAST::Pragma.new requires `name`"))?;
        let ValueView::Str(name) = name.view() else {
            return Err(RuntimeError::new(
                "RakuAST::Pragma.new expects `name` to be a Str",
            ));
        };
        if named_arg(args, "argument").is_some() || named_arg(args, "off").is_some() {
            return Err(RuntimeError::new(
                "RakuAST::Pragma.new only supports argument-less pragmas",
            ));
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::Pragma,
            fields: vec![RakuAstField {
                name: Some("name"),
                value: RakuAstFieldValue::Node(Value::str(name.to_string())),
            }],
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
        let traits = named_arg(args, "traits")
            .map(|value| {
                value.as_list_items().map(<[Value]>::to_vec).ok_or_else(|| {
                    RuntimeError::new("RakuAST::Sub.new expects `traits` to be a list")
                })
            })
            .transpose()?;
        if let Some(traits) = &traits {
            for trait_node in traits {
                if !matches!(
                    trait_node.view(),
                    ValueView::RakuAst(node)
                        if matches!(node.class, RakuAstClass::TraitReturns | RakuAstClass::TraitOf)
                ) {
                    return Err(RuntimeError::new(
                        "RakuAST::Sub.new expects `traits` to contain return-type traits",
                    ));
                }
            }
        }
        let body = match named_arg(args, "body") {
            Some(value) => {
                require_rakuast_class(&value, RakuAstClass::Blockoid, "RakuAST::Sub.new")?;
                value
            }
            None => empty_blockoid(),
        };
        let mut fields = Vec::with_capacity(4);
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
        if let Some(traits) = traits {
            fields.push(RakuAstField {
                name: Some("traits"),
                value: RakuAstFieldValue::List(traits),
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
        let returns = named_arg(args, "returns");
        if let Some(returns) = &returns {
            require_any_rakuast(returns, "RakuAST::Signature.new", "returns")?;
        }
        let mut fields = vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(parameters),
        }];
        if let Some(returns) = returns {
            fields.push(RakuAstField {
                name: Some("returns"),
                value: RakuAstFieldValue::Node(returns),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::Signature,
            fields,
        }))));
    }
    if class_name == "RakuAST::Parameter" && method == "new" {
        let target = named_arg(args, "target");
        if let Some(target) = &target {
            require_rakuast_class(
                target,
                RakuAstClass::ParameterTargetVar,
                "RakuAST::Parameter.new",
            )?;
        }
        let mut fields = Vec::with_capacity(7);
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
        if let Some(type_captures) = named_arg(args, "type-captures") {
            let type_captures = type_captures
                .as_list_items()
                .map(<[Value]>::to_vec)
                .ok_or_else(|| {
                    RuntimeError::new("RakuAST::Parameter.new expects `type-captures` to be a list")
                })?;
            for type_capture in &type_captures {
                require_rakuast_class(
                    type_capture,
                    RakuAstClass::TypeCapture,
                    "RakuAST::Parameter.new",
                )?;
            }
            fields.push(RakuAstField {
                name: Some("type-captures"),
                value: RakuAstFieldValue::List(type_captures),
            });
        }
        if let Some(target) = target {
            fields.push(RakuAstField {
                name: Some("target"),
                value: RakuAstFieldValue::Node(target),
            });
        }
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
        if let Some(sub_signature) = named_arg(args, "sub-signature") {
            require_rakuast_class(
                &sub_signature,
                RakuAstClass::Signature,
                "RakuAST::Parameter.new",
            )?;
            fields.push(RakuAstField {
                name: Some("sub-signature"),
                value: RakuAstFieldValue::Node(sub_signature),
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
    if matches!(
        class_name,
        "RakuAST::Regex::Sequence" | "RakuAST::Regex::Alternation"
    ) && method == "new"
    {
        let class = if class_name.ends_with("Sequence") {
            RakuAstClass::RegexSequence
        } else {
            RakuAstClass::RegexAlternation
        };
        for argument in args {
            require_regex_node(argument, class_name)?;
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class,
            fields: args
                .iter()
                .cloned()
                .map(|value| RakuAstField {
                    name: None,
                    value: RakuAstFieldValue::Node(value),
                })
                .collect(),
        }))));
    }
    if class_name == "RakuAST::Regex::QuantifiedAtom" && method == "new" {
        let atom = named_arg(args, "atom").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::QuantifiedAtom.new requires `atom`")
        })?;
        let quantifier = named_arg(args, "quantifier").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::QuantifiedAtom.new requires `quantifier`")
        })?;
        require_regex_node(&atom, class_name)?;
        require_rakuast_class(
            &quantifier,
            RakuAstClass::RegexQuantifierZeroOrMore,
            "RakuAST::Regex::QuantifiedAtom.new",
        )
        .or_else(|_| {
            require_rakuast_class(
                &quantifier,
                RakuAstClass::RegexQuantifierOneOrMore,
                "RakuAST::Regex::QuantifiedAtom.new",
            )
        })
        .or_else(|_| {
            require_rakuast_class(
                &quantifier,
                RakuAstClass::RegexQuantifierZeroOrOne,
                "RakuAST::Regex::QuantifiedAtom.new",
            )
        })?;
        let mut fields = vec![
            RakuAstField {
                name: Some("atom"),
                value: RakuAstFieldValue::Node(atom),
            },
            RakuAstField {
                name: Some("quantifier"),
                value: RakuAstFieldValue::Node(quantifier),
            },
        ];
        for name in ["separator", "trailing-separator"] {
            if let Some(value) = named_arg(args, name) {
                require_regex_node(&value, class_name)?;
                fields.push(RakuAstField {
                    name: Some(name),
                    value: RakuAstFieldValue::Node(value),
                });
            }
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexQuantifiedAtom,
            fields,
        }))));
    }
    if class_name == "RakuAST::Regex::Interpolation" && method == "new" {
        let var = named_arg(args, "var")
            .ok_or_else(|| RuntimeError::new("RakuAST::Regex::Interpolation.new requires `var`"))?;
        require_rakuast_class(
            &var,
            RakuAstClass::VarLexical,
            "RakuAST::Regex::Interpolation.new",
        )?;
        let sequential = named_arg(args, "sequential").unwrap_or_else(|| Value::truth(false));
        if !matches!(sequential.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::Interpolation.new expects `sequential` to be Bool",
            ));
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexInterpolation,
            fields: vec![
                RakuAstField {
                    name: Some("sequential"),
                    value: RakuAstFieldValue::Node(sequential),
                },
                RakuAstField {
                    name: Some("var"),
                    value: RakuAstFieldValue::Node(var),
                },
            ],
        }))));
    }
    if class_name == "RakuAST::Regex::NamedCapture" && method == "new" {
        let name = named_arg(args, "name")
            .ok_or_else(|| RuntimeError::new("RakuAST::Regex::NamedCapture.new requires `name`"))?;
        if !matches!(name.view(), ValueView::Str(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::NamedCapture.new expects `name` to be Str",
            ));
        }
        let regex = named_arg(args, "regex").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::NamedCapture.new requires `regex`")
        })?;
        require_regex_node(&regex, class_name)?;
        let array = named_arg(args, "array").unwrap_or_else(|| Value::truth(false));
        if !matches!(array.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::NamedCapture.new expects `array` to be Bool",
            ));
        }
        let mut fields = vec![RakuAstField {
            name: Some("name"),
            value: RakuAstFieldValue::Node(name),
        }];
        if matches!(array.view(), ValueView::Bool(true)) {
            fields.push(RakuAstField {
                name: Some("array"),
                value: RakuAstFieldValue::Node(array),
            });
        }
        fields.push(RakuAstField {
            name: Some("regex"),
            value: RakuAstFieldValue::Node(regex),
        });
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexNamedCapture,
            fields,
        }))));
    }
    if class_name == "RakuAST::QuotedRegex" && method == "new" {
        let body = named_arg(args, "body")
            .ok_or_else(|| RuntimeError::new("RakuAST::QuotedRegex.new requires `body`"))?;
        require_regex_node(&body, class_name)?;
        let match_immediately = named_arg(args, "match-immediately");
        if let Some(value) = &match_immediately
            && !matches!(value.view(), ValueView::Bool(_))
        {
            return Err(RuntimeError::new(
                "RakuAST::QuotedRegex.new expects `match-immediately` to be Bool",
            ));
        }
        let adverbs = named_arg(args, "adverbs")
            .map(|value| {
                value.as_list_items().map(<[Value]>::to_vec).ok_or_else(|| {
                    RuntimeError::new("RakuAST::QuotedRegex.new expects `adverbs` to be a list")
                })
            })
            .transpose()?
            .unwrap_or_default();
        for adverb in &adverbs {
            require_rakuast_class(
                adverb,
                RakuAstClass::ColonPairTrue,
                "RakuAST::QuotedRegex.new",
            )?;
        }
        let mut fields = Vec::new();
        if let Some(value) = match_immediately {
            fields.push(RakuAstField {
                name: Some("match-immediately"),
                value: RakuAstFieldValue::Node(value),
            });
        }
        fields.push(RakuAstField {
            name: Some("body"),
            value: RakuAstFieldValue::Node(body),
        });
        if !adverbs.is_empty() {
            fields.push(RakuAstField {
                name: Some("adverbs"),
                value: RakuAstFieldValue::List(adverbs),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::QuotedRegex,
            fields,
        }))));
    }
    if matches!(
        class_name,
        "RakuAST::RegexDeclaration" | "RakuAST::TokenDeclaration" | "RakuAST::RuleDeclaration"
    ) && method == "new"
    {
        let name = named_arg(args, "name")
            .ok_or_else(|| RuntimeError::new(format!("{class_name}.new requires `name`")))?;
        require_rakuast_class(&name, RakuAstClass::Name, "RakuAST regex declaration")?;
        let body = named_arg(args, "body")
            .ok_or_else(|| RuntimeError::new(format!("{class_name}.new requires `body`")))?;
        require_regex_node(&body, class_name)?;
        let class = match class_name {
            "RakuAST::RegexDeclaration" => RakuAstClass::RegexDeclaration,
            "RakuAST::TokenDeclaration" => RakuAstClass::TokenDeclaration,
            _ => RakuAstClass::RuleDeclaration,
        };
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class,
            fields: vec![
                RakuAstField {
                    name: Some("name"),
                    value: RakuAstFieldValue::Node(name),
                },
                RakuAstField {
                    name: Some("body"),
                    value: RakuAstFieldValue::Node(body),
                },
            ],
        }))));
    }
    if class_name == "RakuAST::Grammar" && method == "new" {
        let name = named_arg(args, "name")
            .ok_or_else(|| RuntimeError::new("RakuAST::Grammar.new requires `name`"))?;
        require_rakuast_class(&name, RakuAstClass::Name, "RakuAST::Grammar.new")?;
        if named_arg(args, "body").is_some() {
            return Err(RuntimeError::new(
                "RakuAST::Grammar.new does not accept a `body` argument",
            ));
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::Grammar,
            fields: vec![
                RakuAstField {
                    name: Some("name"),
                    value: RakuAstFieldValue::Node(name),
                },
                RakuAstField {
                    name: Some("body"),
                    value: RakuAstFieldValue::Node(empty_block()),
                },
            ],
        }))));
    }
    if let Some(class) = zero_positional_class(class_name, method) {
        if !args.is_empty() {
            return Err(RuntimeError::new(format!(
                "{class_name}.{method} expects no arguments"
            )));
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class,
            fields: Vec::new(),
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

fn empty_block() -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::Block,
        fields: vec![RakuAstField {
            name: Some("body"),
            value: RakuAstFieldValue::Node(empty_blockoid()),
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

fn require_regex_node(value: &Value, constructor: &str) -> Result<(), RuntimeError> {
    match value.view() {
        ValueView::RakuAst(node)
            if matches!(
                node.class,
                RakuAstClass::RegexSequence
                    | RakuAstClass::RegexLiteral
                    | RakuAstClass::RegexQuote
                    | RakuAstClass::RegexWithWhitespace
                    | RakuAstClass::RegexGroup
                    | RakuAstClass::RegexCapturingGroup
                    | RakuAstClass::RegexNamedCapture
                    | RakuAstClass::RegexInterpolation
                    | RakuAstClass::RegexAlternation
                    | RakuAstClass::RegexQuantifiedAtom
                    | RakuAstClass::RegexCharClassDigit
            ) =>
        {
            Ok(())
        }
        _ => Err(RuntimeError::new(format!(
            "{constructor} expects a RakuAST regex node"
        ))),
    }
}

fn require_rakuast_type(value: &Value, constructor: &str) -> Result<(), RuntimeError> {
    match value.view() {
        ValueView::RakuAst(node)
            if matches!(
                node.class,
                RakuAstClass::TypeSimple
                    | RakuAstClass::TypeEnum
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

/// The value a `Parameter`'s `slurpy` field holds: the
/// `RakuAST::Parameter::Slurpy::*` **TYPE OBJECT**, not a node of that class.
///
/// Rakudo builds no node for a slurpy marker -- the type object itself is
/// stored in `$!slurpy` -- and the difference is visible on the field even
/// though the two render identically inside the parent's gist. mutsu used to
/// normalize the other way (type object in, empty node out), which made
/// `$p.slurpy.defined` answer `True` for every slurpy parameter where rakudo
/// answers `False`, `$p.slurpy.gist` the full class name where rakudo gists
/// `(Flattened)`, and `$p.slurpy === RakuAST::Parameter::Slurpy::Flattened`
/// `False` where rakudo says `True`. On rakudo the test for "is this parameter
/// slurpy?" is which CLASS of type object the field holds, never definedness
/// (GH #8157).
///
/// Both spellings are accepted on the way in: rakudo's `.new` takes only the
/// type object, and that is what `t/rakuast/rakuast-construct-rich-parameters.t`
/// passes, but a node of the same class is an unambiguous way to name the same
/// marker and there is nothing to gain from rejecting it.
pub(crate) fn slurpy_marker_value(class: RakuAstClass) -> Value {
    Value::package(crate::symbol::Symbol::intern(class.printed_name()))
}

/// The slurpy class a field's value names, whichever of the two spellings it
/// uses. `None` for anything that is not a slurpy marker.
pub(crate) fn slurpy_marker_class(value: &Value) -> Option<RakuAstClass> {
    match value.view() {
        ValueView::RakuAst(node)
            if matches!(
                node.class,
                RakuAstClass::ParameterSlurpyFlattened | RakuAstClass::ParameterSlurpyUnflattened
            ) =>
        {
            Some(node.class)
        }
        ValueView::Package(name) => match name.resolve().as_str() {
            "RakuAST::Parameter::Slurpy::Flattened" => Some(RakuAstClass::ParameterSlurpyFlattened),
            "RakuAST::Parameter::Slurpy::Unflattened" => {
                Some(RakuAstClass::ParameterSlurpyUnflattened)
            }
            _ => None,
        },
        _ => None,
    }
}

fn normalize_slurpy_marker(value: Value) -> Result<Value, RuntimeError> {
    slurpy_marker_class(&value)
        .map(slurpy_marker_value)
        .ok_or_else(|| RuntimeError::new("RakuAST::Parameter.new expects a RakuAST slurpy marker"))
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
        ("RakuAST::FunctionInfix", "new") => RakuAstClass::FunctionInfix,
        ("RakuAST::MetaInfix::Assign", "new") => RakuAstClass::MetaInfixAssign,
        ("RakuAST::Prefix", "new") => RakuAstClass::Prefix,
        ("RakuAST::Var::Lexical", "new") => RakuAstClass::VarLexical,
        ("RakuAST::Initializer::Assign", "new") => RakuAstClass::InitializerAssign,
        ("RakuAST::Type::Simple", "new") => RakuAstClass::TypeSimple,
        ("RakuAST::Type::Setting", "new") => RakuAstClass::TypeSetting,
        ("RakuAST::Type::Capture", "new") => RakuAstClass::TypeCapture,
        ("RakuAST::Trait::Returns", "new") => RakuAstClass::TraitReturns,
        ("RakuAST::Trait::Of", "new") => RakuAstClass::TraitOf,
        ("RakuAST::Regex::Literal", "new") => RakuAstClass::RegexLiteral,
        ("RakuAST::Regex::Quote", "new") => RakuAstClass::RegexQuote,
        ("RakuAST::Regex::Group", "new") => RakuAstClass::RegexGroup,
        ("RakuAST::Regex::CapturingGroup", "new") => RakuAstClass::RegexCapturingGroup,
        ("RakuAST::Regex::WithWhitespace", "new") => RakuAstClass::RegexWithWhitespace,
        ("RakuAST::ColonPair::True", "new") => RakuAstClass::ColonPairTrue,
        _ => return None,
    })
}

fn zero_positional_class(class_name: &str, method: &str) -> Option<RakuAstClass> {
    Some(match (class_name, method) {
        ("RakuAST::Regex::Quantifier::ZeroOrMore", "new") => {
            RakuAstClass::RegexQuantifierZeroOrMore
        }
        ("RakuAST::Regex::Quantifier::OneOrMore", "new") => RakuAstClass::RegexQuantifierOneOrMore,
        ("RakuAST::Regex::Quantifier::ZeroOrOne", "new") => RakuAstClass::RegexQuantifierZeroOrOne,
        ("RakuAST::Regex::CharClass::Digit", "new") => RakuAstClass::RegexCharClassDigit,
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
    // with a *named* field of the same name (e.g. `Call::Name.name`) is
    // unaffected.
    if fields::positional_accessor(node.class) == Some(method)
        && let Some(f) = node.fields.first()
        && f.name.is_none()
    {
        return Some(field_to_value(&f.value));
    }
    // A field the class DECLARES but this node does not carry still answers:
    // rakudo models every field as an attribute, so an absent optional clause
    // reads as an undefined type object (or `()` / `False` / `0`) rather than
    // dying. That is what makes `.defined` the way to test for one — see
    // `fields::Absent`.
    fields::model_fields(node.class)
        .iter()
        .find(|(name, _)| *name == method)
        .and_then(|(_, absent)| absent.value())
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

/// The public methods a RakuAST model class exposes *including* the ones it
/// inherits, for `.^methods` (whose default, per
/// `Type/Metamodel/MethodContainer.rakudoc`, is "methods of the class and its
/// parents, stopping at Cool/Any/Mu" — `:local` is the narrower set
/// [`local_method_names`] returns). Walks the model MRO and drops the `Any` /
/// `Mu` tail, so the result grows automatically as the abstract RakuAST classes
/// gain model methods of their own.
pub fn inherited_method_names(class_name: &str) -> Option<Vec<&'static str>> {
    let mro = type_object_mro(class_name)?;
    let mut names = Vec::new();
    for cls in &mro {
        if cls == "Any" || cls == "Mu" {
            break;
        }
        if let Some(local) = local_method_names(cls) {
            names.extend(local);
        }
    }
    names.sort_unstable();
    names.dedup();
    Some(names)
}

/// Model fields declared directly by a RakuAST class, for `.^attributes(:local)`.
///
/// As with [`local_method_names`], these are mutsu's public model fields rather
/// than Rakudo's backend storage slots.
pub fn local_attribute_names(class_name: &str) -> Option<Vec<&'static str>> {
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
            | RakuAstClass::FunctionInfix
            | RakuAstClass::Postfix
            | RakuAstClass::MetaInfixAssign
            | RakuAstClass::Block
            | RakuAstClass::Blockoid
            | RakuAstClass::Sub
            | RakuAstClass::Signature
            | RakuAstClass::TraitReturns
            | RakuAstClass::TraitOf
            | RakuAstClass::Parameter
            | RakuAstClass::ParameterTargetVar
            | RakuAstClass::VarDeclarationSimple
            | RakuAstClass::InitializerAssign
            | RakuAstClass::TypeSimple
            | RakuAstClass::TypeEnum
            | RakuAstClass::TypeSetting
            | RakuAstClass::TypeCapture
            | RakuAstClass::QuotedRegex
            | RakuAstClass::RegexSequence
            | RakuAstClass::RegexAlternation
            | RakuAstClass::RegexLiteral
            | RakuAstClass::RegexQuote
            | RakuAstClass::RegexWithWhitespace
            | RakuAstClass::RegexGroup
            | RakuAstClass::RegexCapturingGroup
            | RakuAstClass::RegexNamedCapture
            | RakuAstClass::RegexInterpolation
            | RakuAstClass::RegexQuantifiedAtom
            | RakuAstClass::RegexQuantifierZeroOrMore
            | RakuAstClass::RegexQuantifierOneOrMore
            | RakuAstClass::RegexQuantifierZeroOrOne
            | RakuAstClass::RegexCharClassDigit
            | RakuAstClass::ColonPairTrue
            | RakuAstClass::RegexDeclaration
            | RakuAstClass::TokenDeclaration
            | RakuAstClass::RuleDeclaration
            | RakuAstClass::Grammar
            | RakuAstClass::Pragma
    )
}

/// The accessor names a RakuAST class declares, in declaration order. Derived
/// from [`fields::model_fields`] so introspection and dispatch cannot disagree
/// about which accessors a class has.
fn accessor_names(class: RakuAstClass) -> Vec<&'static str> {
    fields::model_fields(class)
        .iter()
        .map(|(name, _)| *name)
        .collect()
}

fn field_to_value(fv: &RakuAstFieldValue) -> Value {
    match fv {
        RakuAstFieldValue::Node(v) => v.clone(),
        RakuAstFieldValue::List(items) => Value::array(items.clone()),
        // Colonpair adverbs (`:item`) are a rendering detail; expose as True.
        RakuAstFieldValue::Adverb(_) => Value::truth(true),
    }
}
