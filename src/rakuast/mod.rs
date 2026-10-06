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

mod anon_state;
mod atomic_op;
mod attribute;
mod bareword;
mod capture_term;
mod chain;
mod contextualizer;
mod convert;
mod core_term_names;
mod core_type_names;
mod decl_traits;
mod declared_routines;
mod dynamic_method;
mod feed_op;
mod fields;
mod formatter;
pub(crate) mod frontend;
mod hash_literal;
mod infix_func;
mod keyed_hash;
mod lower;
mod match_vars;
mod meta_infix;
mod method_assign_decl;
mod name_parts;
mod named_param;
mod origin;
mod package_header;
mod placeholder;
mod proto;
mod react;
mod regex_char_class;
mod regex_code;
mod regex_enumeration;
mod regex_extension;
mod regex_quantifier;
mod render;
mod role;
mod routine_traits;
mod shadowed_terms;
mod signature_decl;
mod subscript_adverb;
mod substitution;
mod symbolic_deref;
mod temporize;
mod type_args;
mod type_lower;
mod use_stmt;

pub use formatter::formatter_ast;
pub use lower::lower;
pub(crate) use name_parts::{is_name_part, is_name_part_class};

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
    CompUnit,
    StatementList,
    StatementExpression,
    StatementAlso,
    StatementPrefixReact,
    StatementPrefixSupply,
    StatementWhenever,
    OnlyStar,
    IntLiteral,
    NumLiteral,
    RatLiteral,
    // `v6.d` / `<1+2i>`: the value is the node's one positional field.
    VersionLiteral,
    ComplexLiteral,
    StrLiteral,
    QuotedString,
    QuotedRegex,
    RegexSequence,
    RegexLiteral,
    RegexQuote,
    RegexWithWhitespace,
    RegexBlock,
    RegexGroup,
    RegexCapturingGroup,
    RegexNamedCapture,
    RegexAssertionNamed,
    RegexAssertionNamedArgs,
    RegexAssertionAlias,
    RegexAssertionNamedRegexArg,
    RegexAssertionLookahead,
    RegexAssertionInterpolatedVar,
    RegexAssertionCallable,
    RegexAssertionPredicateBlock,
    RegexAssertionInterpolatedBlock,
    RegexInterpolation,
    RegexAlternation,
    RegexSequentialAlternation,
    RegexQuantifiedAtom,
    RegexQuantifierZeroOrMore,
    RegexQuantifierOneOrMore,
    RegexQuantifierZeroOrOne,
    RegexAnchorBeginningOfString,
    RegexAnchorBeginningOfLine,
    RegexAnchorEndOfString,
    RegexAnchorEndOfLine,
    RegexAnchorLeftWordBoundary,
    RegexAnchorRightWordBoundary,
    RegexMatchFrom,
    RegexAssertionPass,
    RegexAssertionFail,
    RegexMatchTo,
    RegexQuantifierRange,
    RegexBacktrackFrugal,
    RegexBacktrackGreedy,
    RegexBacktrackRatchet,
    RegexCharClass(regex_char_class::RegexCharClassKind),
    RegexAssertionCharClass,
    RegexCharClassElementEnumeration,
    RegexCharClassElementRule,
    RegexCharClassElementProperty,
    RegexCharClassEnumerationElementCharacter,
    RegexCharClassEnumerationElementRange,
    RegexInternalModifierIgnoreCase,
    RegexInternalModifierIgnoreMark,
    RegexInternalModifierSigspace,
    RegexInternalModifierRatchet,
    ColonPairTrue,
    ColonPairFalse,
    ColonPairVariable,
    ColonPairValue,
    RegexDeclaration,
    TokenDeclaration,
    RuleDeclaration,
    Grammar,
    CallName,
    CallNameWithoutParentheses,
    Name,
    // A static qualified name, as in `Name.from-identifier-parts("G", "foo")`.
    NamePartSimple,
    // A dynamic name part, as in `::("x")`, is not itself a RakuAST::Node in
    // Rakudo, but it is carried by the same model value here so the immutable
    // tree can retain the exact Name shape.
    NamePartExpression,
    // The empty edge of a name: the leading `::` of `::Foo` / `::($x)` and the
    // trailing `::` of a stash lookup `Foo::`. Rakudo 2026.09 spells it
    // `Name::Part::Empty`; rakudo/rakudo#6771 renames it to
    // `Name::Part::EmptyEdge`, so both spellings are modelled and accepted.
    NamePartEmpty,
    NamePartEmptyEdge,
    ArgList,
    // Phase 2: variables, declarations, operators.
    VarLexical,
    // A package-qualified variable `$Foo::v`: a `Name` plus its sigil.
    VarPackage,
    // A dynamic variable `$*x`: its whole spelling, twigil included.
    VarDynamic,
    VarDeclarationSimple,
    InitializerAssign,
    InitializerCallAssign,
    VarDeclarationSignature,
    InitializerBind,
    ApplyInfix,
    Infix,
    FunctionInfix,
    ApplyPrefix,
    Prefix,
    ApplyPostfix,
    Postfix,
    Assignment,
    MetaInfixAssign,
    RegexAssertionRecurse,
    RegexBackReferencePositional,
    RegexBackReferenceNamed,
    RegexStatement,
    RegexNested,
    VarPositionalCapture,
    ColonPairNumber,
    Transliteration,
    Substitution,
    VarNamedCapture,
    CallNameAsMethod,
    CallTermAsMethod,
    FlipFlop,
    Feed,
    StatementPrefixEager,
    TermCapture,
    MetaInfixReverse,
    MetaInfixCross,
    MetaInfixZip,
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
    VarDeclarationPlaceholderPositional,
    VarDeclarationPlaceholderNamed,
    VarDeclarationPlaceholderSlurpyArray,
    VarDeclarationPlaceholderSlurpyHash,
    Signature,
    Parameter,
    ParameterTargetVar,
    ParameterTargetTerm,
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
    // `$x .= meth`: `ApplyDottyInfix(left, DottyInfix::CallAssign, Call::Method)`.
    ApplyDottyInfix,
    DottyInfixCallAssign,
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
    StatementModifierFor,
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
    PostcircumfixHashIndex,
    PostcircumfixLiteralHashIndex,
    // Phase 2 slice 25: reduction metaoperator.
    TermReduce,
    // Phase 2 slice 30: `True`/`False` (and other enum) literals.
    TermEnum,
    // Phase 2 slice 31: parenthesised expressions (`($x = 5)`).
    CircumfixParentheses,
    // Phase 2 slice 32: slurpy parameter markers (`*@a` / `**@a`).
    ParameterSlurpyFlattened,
    ParameterSlurpyUnflattened,
    // `+a` / `+@a` and `|c` / `|`.
    ParameterSlurpySingleArgument,
    ParameterSlurpyCapture,
    // Phase 2 slice 33: array-composer literal (`[1, 2, 3]`).
    CircumfixArrayComposer,
    // A hash composer `{a => 1}` and the `%(…)` hash contextualizer, whose
    // contents are a `StatementSequence`.
    CircumfixHashComposer,
    ContextualizerHash,
    ContextualizerItem,
    ContextualizerList,
    StatementSequence,
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
    // `my \x = 5` / `my \x := $s`.
    VarDeclarationTerm,
    // A bare `$` / `@` / `%`: an anonymous `state` variable.
    VarDeclarationAnonymous,
    // A bareword naming something the unit declared that is not a type — a
    // `constant`, in practice.
    TermName,
    // A named setting term (`now`): `Term::Named.new("now")`, one positional string.
    TermNamed,
    // `.method` on the topic: a positional `Call::Method`. Write direction
    // only -- the parser does not yet tell `.uc` from `$_.uc`.
    TermTopicCall,
    // `.^name` — a metamethod call, distinct from `.?`/`.+`/`.*` dispatch.
    CallMetaMethod,
    // `until` / `repeat … until` — raku's own classes, not a negated `while`.
    StatementLoopUntil,
    StatementLoopRepeatUntil,
    // Class/role declaration traits (`is Parent`, `does Role`, `is rw`).
    TraitIs,
    /// `handles TERM` on an attribute.
    TraitHandles,
    TraitDoes,
    /// `class A hides B`.
    TraitHides,
    /// `trusts B` in a class body.
    StatementTrusts,
    StatementPrefixOnce,
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
    StatementControl,
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
    /// `...` / `!!!` / `???` (the yada-yada stubs).
    StubFail,
    StubDie,
    StubWarn,
    // An argument-less core `use` pragma (`use strict`, `use fatal`, ...).
    Pragma,
    StatementUse,
    // `need Module;` / `import Module :tag;`.
    StatementNeed,
    StatementImport,
    StatementLanguageVersion,
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
            CompUnit => "RakuAST::CompUnit",
            StatementList => "RakuAST::StatementList",
            StatementExpression => "RakuAST::Statement::Expression",
            StatementAlso => "RakuAST::Statement::Also",
            StatementPrefixReact => "RakuAST::StatementPrefix::React",
            StatementPrefixSupply => "RakuAST::StatementPrefix::Supply",
            StatementWhenever => "RakuAST::Statement::Whenever",
            OnlyStar => "RakuAST::OnlyStar",
            IntLiteral => "RakuAST::IntLiteral",
            NumLiteral => "RakuAST::NumLiteral",
            RatLiteral => "RakuAST::RatLiteral",
            VersionLiteral => "RakuAST::VersionLiteral",
            ComplexLiteral => "RakuAST::ComplexLiteral",
            StrLiteral => "RakuAST::StrLiteral",
            QuotedString => "RakuAST::QuotedString",
            QuotedRegex => "RakuAST::QuotedRegex",
            RegexSequence => "RakuAST::Regex::Sequence",
            RegexLiteral => "RakuAST::Regex::Literal",
            RegexQuote => "RakuAST::Regex::Quote",
            RegexWithWhitespace => "RakuAST::Regex::WithWhitespace",
            RegexBlock => "RakuAST::Regex::Block",
            RegexGroup => "RakuAST::Regex::Group",
            RegexCapturingGroup => "RakuAST::Regex::CapturingGroup",
            RegexNamedCapture => "RakuAST::Regex::NamedCapture",
            RegexAssertionNamed => "RakuAST::Regex::Assertion::Named",
            RegexAssertionNamedArgs => "RakuAST::Regex::Assertion::Named::Args",
            RegexAssertionAlias => "RakuAST::Regex::Assertion::Alias",
            RegexAssertionNamedRegexArg => "RakuAST::Regex::Assertion::Named::RegexArg",
            RegexAssertionLookahead => "RakuAST::Regex::Assertion::Lookahead",
            RegexAssertionInterpolatedVar => "RakuAST::Regex::Assertion::InterpolatedVar",
            RegexAssertionCallable => "RakuAST::Regex::Assertion::Callable",
            RegexAssertionPredicateBlock => "RakuAST::Regex::Assertion::PredicateBlock",
            RegexAssertionInterpolatedBlock => "RakuAST::Regex::Assertion::InterpolatedBlock",
            RegexInterpolation => "RakuAST::Regex::Interpolation",
            RegexAlternation => "RakuAST::Regex::Alternation",
            RegexSequentialAlternation => "RakuAST::Regex::SequentialAlternation",
            RegexQuantifiedAtom => "RakuAST::Regex::QuantifiedAtom",
            RegexQuantifierZeroOrMore => "RakuAST::Regex::Quantifier::ZeroOrMore",
            RegexQuantifierOneOrMore => "RakuAST::Regex::Quantifier::OneOrMore",
            RegexQuantifierZeroOrOne => "RakuAST::Regex::Quantifier::ZeroOrOne",
            RegexAnchorBeginningOfString => "RakuAST::Regex::Anchor::BeginningOfString",
            RegexAnchorBeginningOfLine => "RakuAST::Regex::Anchor::BeginningOfLine",
            RegexAnchorEndOfString => "RakuAST::Regex::Anchor::EndOfString",
            RegexAnchorEndOfLine => "RakuAST::Regex::Anchor::EndOfLine",
            RegexAnchorLeftWordBoundary => "RakuAST::Regex::Anchor::LeftWordBoundary",
            RegexAnchorRightWordBoundary => "RakuAST::Regex::Anchor::RightWordBoundary",
            RegexMatchFrom => "RakuAST::Regex::MatchFrom",
            RegexAssertionPass => "RakuAST::Regex::Assertion::Pass",
            RegexAssertionFail => "RakuAST::Regex::Assertion::Fail",
            RegexMatchTo => "RakuAST::Regex::MatchTo",
            RegexQuantifierRange => "RakuAST::Regex::Quantifier::Range",
            RegexBacktrackFrugal => "RakuAST::Regex::Backtrack::Frugal",
            RegexBacktrackGreedy => "RakuAST::Regex::Backtrack::Greedy",
            RegexBacktrackRatchet => "RakuAST::Regex::Backtrack::Ratchet",
            RegexCharClass(kind) => kind.printed_name(),
            RegexAssertionCharClass => "RakuAST::Regex::Assertion::CharClass",
            RegexCharClassElementEnumeration => "RakuAST::Regex::CharClassElement::Enumeration",
            RegexCharClassElementRule => "RakuAST::Regex::CharClassElement::Rule",
            RegexCharClassElementProperty => "RakuAST::Regex::CharClassElement::Property",
            RegexCharClassEnumerationElementCharacter => {
                "RakuAST::Regex::CharClassEnumerationElement::Character"
            }
            RegexCharClassEnumerationElementRange => {
                "RakuAST::Regex::CharClassEnumerationElement::Range"
            }
            RegexInternalModifierIgnoreCase => "RakuAST::Regex::InternalModifier::IgnoreCase",
            RegexInternalModifierIgnoreMark => "RakuAST::Regex::InternalModifier::IgnoreMark",
            RegexInternalModifierSigspace => "RakuAST::Regex::InternalModifier::Sigspace",
            RegexInternalModifierRatchet => "RakuAST::Regex::InternalModifier::Ratchet",
            ColonPairTrue => "RakuAST::ColonPair::True",
            ColonPairFalse => "RakuAST::ColonPair::False",
            ColonPairVariable => "RakuAST::ColonPair::Variable",
            ColonPairValue => "RakuAST::ColonPair::Value",
            RegexDeclaration => "RakuAST::RegexDeclaration",
            TokenDeclaration => "RakuAST::TokenDeclaration",
            RuleDeclaration => "RakuAST::RuleDeclaration",
            Grammar => "RakuAST::Grammar",
            CallName => "RakuAST::Call::Name",
            CallNameWithoutParentheses => "RakuAST::Call::Name::WithoutParentheses",
            Name => "RakuAST::Name",
            NamePartSimple => "RakuAST::Name::Part::Simple",
            NamePartExpression => "RakuAST::Name::Part::Expression",
            NamePartEmpty => "RakuAST::Name::Part::Empty",
            NamePartEmptyEdge => "RakuAST::Name::Part::EmptyEdge",
            ArgList => "RakuAST::ArgList",
            VarLexical => "RakuAST::Var::Lexical",
            VarPackage => "RakuAST::Var::Package",
            VarDynamic => "RakuAST::Var::Dynamic",
            VarDeclarationSimple => "RakuAST::VarDeclaration::Simple",
            InitializerAssign => "RakuAST::Initializer::Assign",
            InitializerCallAssign => "RakuAST::Initializer::CallAssign",
            VarDeclarationSignature => "RakuAST::VarDeclaration::Signature",
            InitializerBind => "RakuAST::Initializer::Bind",
            ApplyInfix => "RakuAST::ApplyInfix",
            Infix => "RakuAST::Infix",
            FunctionInfix => "RakuAST::FunctionInfix",
            ApplyPrefix => "RakuAST::ApplyPrefix",
            Prefix => "RakuAST::Prefix",
            ApplyPostfix => "RakuAST::ApplyPostfix",
            Postfix => "RakuAST::Postfix",
            Assignment => "RakuAST::Assignment",
            MetaInfixAssign => "RakuAST::MetaInfix::Assign",
            RegexAssertionRecurse => "RakuAST::Regex::Assertion::Recurse",
            RegexBackReferencePositional => "RakuAST::Regex::BackReference::Positional",
            RegexBackReferenceNamed => "RakuAST::Regex::BackReference::Named",
            RegexStatement => "RakuAST::Regex::Statement",
            RegexNested => "RakuAST::Regex::Nested",
            VarPositionalCapture => "RakuAST::Var::PositionalCapture",
            ColonPairNumber => "RakuAST::ColonPair::Number",
            Transliteration => "RakuAST::Transliteration",
            Substitution => "RakuAST::Substitution",
            VarNamedCapture => "RakuAST::Var::NamedCapture",
            CallNameAsMethod => "RakuAST::Call::NameAsMethod",
            CallTermAsMethod => "RakuAST::Call::TermAsMethod",
            FlipFlop => "RakuAST::FlipFlop",
            Feed => "RakuAST::Feed",
            StatementPrefixEager => "RakuAST::StatementPrefix::Eager",
            TermCapture => "RakuAST::Term::Capture",
            MetaInfixReverse => "RakuAST::MetaInfix::Reverse",
            MetaInfixCross => "RakuAST::MetaInfix::Cross",
            MetaInfixZip => "RakuAST::MetaInfix::Zip",
            CallMethod => "RakuAST::Call::Method",
            CallQuotedMethod => "RakuAST::Call::QuotedMethod",
            MetaPostfixHyper => "RakuAST::MetaPostfix::Hyper",
            MetaInfixHyper => "RakuAST::MetaInfix::Hyper",
            Block => "RakuAST::Block",
            Blockoid => "RakuAST::Blockoid",
            PointyBlock => "RakuAST::PointyBlock",
            VarDeclarationPlaceholderPositional => {
                "RakuAST::VarDeclaration::Placeholder::Positional"
            }
            VarDeclarationPlaceholderNamed => "RakuAST::VarDeclaration::Placeholder::Named",
            VarDeclarationPlaceholderSlurpyArray => {
                "RakuAST::VarDeclaration::Placeholder::SlurpyArray"
            }
            VarDeclarationPlaceholderSlurpyHash => {
                "RakuAST::VarDeclaration::Placeholder::SlurpyHash"
            }
            Signature => "RakuAST::Signature",
            Parameter => "RakuAST::Parameter",
            ParameterTargetVar => "RakuAST::ParameterTarget::Var",
            ParameterTargetTerm => "RakuAST::ParameterTarget::Term",
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
            ApplyDottyInfix => "RakuAST::ApplyDottyInfix",
            DottyInfixCallAssign => "RakuAST::DottyInfix::CallAssign",
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
            StatementModifierFor => "RakuAST::StatementModifier::For",
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
            PostcircumfixHashIndex => "RakuAST::Postcircumfix::HashIndex",
            PostcircumfixLiteralHashIndex => "RakuAST::Postcircumfix::LiteralHashIndex",
            TermReduce => "RakuAST::Term::Reduce",
            TermEnum => "RakuAST::Term::Enum",
            CircumfixParentheses => "RakuAST::Circumfix::Parentheses",
            ParameterSlurpyFlattened => "RakuAST::Parameter::Slurpy::Flattened",
            ParameterSlurpyUnflattened => "RakuAST::Parameter::Slurpy::Unflattened",
            ParameterSlurpySingleArgument => "RakuAST::Parameter::Slurpy::SingleArgument",
            ParameterSlurpyCapture => "RakuAST::Parameter::Slurpy::Capture",
            CircumfixArrayComposer => "RakuAST::Circumfix::ArrayComposer",
            CircumfixHashComposer => "RakuAST::Circumfix::HashComposer",
            ContextualizerHash => "RakuAST::Contextualizer::Hash",
            ContextualizerItem => "RakuAST::Contextualizer::Item",
            ContextualizerList => "RakuAST::Contextualizer::List",
            StatementSequence => "RakuAST::StatementSequence",
            TermWhatever => "RakuAST::Term::Whatever",
            WhateverCodeArgument => "RakuAST::WhateverCode::Argument",
            TermHyperWhatever => "RakuAST::Term::HyperWhatever",
            FatArrow => "RakuAST::FatArrow",
            StatementPrefixDo => "RakuAST::StatementPrefix::Do",
            StatementPrefixTry => "RakuAST::StatementPrefix::Try",
            StatementPrefixGather => "RakuAST::StatementPrefix::Gather",
            CallTerm => "RakuAST::Call::Term",
            VarDeclarationConstant => "RakuAST::VarDeclaration::Constant",
            VarDeclarationTerm => "RakuAST::VarDeclaration::Term",
            VarDeclarationAnonymous => "RakuAST::VarDeclaration::Anonymous",
            TermName => "RakuAST::Term::Name",
            TermNamed => "RakuAST::Term::Named",
            TermTopicCall => "RakuAST::Term::TopicCall",
            CallMetaMethod => "RakuAST::Call::MetaMethod",
            StatementLoopUntil => "RakuAST::Statement::Loop::Until",
            StatementLoopRepeatUntil => "RakuAST::Statement::Loop::RepeatUntil",
            TraitIs => "RakuAST::Trait::Is",
            TraitHandles => "RakuAST::Trait::Handles",
            TraitDoes => "RakuAST::Trait::Does",
            TraitHides => "RakuAST::Trait::Hides",
            StatementTrusts => "RakuAST::Statement::Trusts",
            StatementPrefixOnce => "RakuAST::StatementPrefix::Once",
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
            StatementControl => "RakuAST::Statement::Control",
            TypeSubset => "RakuAST::Type::Subset",
            Module => "RakuAST::Module",
            Package => "RakuAST::Package",
            Submethod => "RakuAST::Submethod",
            TermSelf => "RakuAST::Term::Self",
            StubFail => "RakuAST::Stub::Fail",
            StubDie => "RakuAST::Stub::Die",
            StubWarn => "RakuAST::Stub::Warn",
            Pragma => "RakuAST::Pragma",
            StatementUse => "RakuAST::Statement::Use",
            StatementNeed => "RakuAST::Statement::Need",
            StatementImport => "RakuAST::Statement::Import",
            StatementLanguageVersion => "RakuAST::Statement::LanguageVersion",
        }
    }

    /// raku's `Assignment` gist omits the empty `()` for the list form
    /// (`RakuAST::Assignment.new`), unlike the generic `.new()` (e.g. an empty
    /// `StatementList` still prints `RakuAST::StatementList.new()`).
    pub fn empty_parens_omitted(self) -> bool {
        matches!(
            self,
            RakuAstClass::Assignment
                | RakuAstClass::DottyInfixCallAssign
                | RakuAstClass::TermWhatever
                | RakuAstClass::WhateverCodeArgument
                | RakuAstClass::TermHyperWhatever
                | RakuAstClass::TermSelf
                | RakuAstClass::StubFail
                | RakuAstClass::StubDie
                | RakuAstClass::StubWarn
                | RakuAstClass::VarDeclarationPlaceholderSlurpyArray
                | RakuAstClass::VarDeclarationPlaceholderSlurpyHash
                | RakuAstClass::RegexQuantifierZeroOrMore
                | RakuAstClass::RegexQuantifierOneOrMore
                | RakuAstClass::RegexQuantifierZeroOrOne
                | RakuAstClass::RegexAnchorBeginningOfString
                | RakuAstClass::RegexAnchorBeginningOfLine
                | RakuAstClass::RegexAnchorEndOfString
                | RakuAstClass::RegexAnchorEndOfLine
                | RakuAstClass::RegexAnchorLeftWordBoundary
                | RakuAstClass::RegexAnchorRightWordBoundary
                | RakuAstClass::RegexMatchFrom
                | RakuAstClass::RegexAssertionPass
                | RakuAstClass::RegexAssertionFail
                | RakuAstClass::RegexAssertionRecurse
                | RakuAstClass::RegexMatchTo
                | RakuAstClass::OnlyStar
                | RakuAstClass::RegexQuantifierRange
                | RakuAstClass::RegexCharClass(_)
                | RakuAstClass::RegexAssertionCharClass
                | RakuAstClass::RegexCharClassElementEnumeration
                | RakuAstClass::RegexCharClassElementRule
                | RakuAstClass::RegexCharClassElementProperty
                | RakuAstClass::RegexCharClassEnumerationElementCharacter
                | RakuAstClass::RegexCharClassEnumerationElementRange
                | RakuAstClass::RegexInternalModifierIgnoreCase
                | RakuAstClass::RegexInternalModifierIgnoreMark
                | RakuAstClass::RegexInternalModifierSigspace
                | RakuAstClass::RegexInternalModifierRatchet
                | RakuAstClass::NamePartEmpty
                | RakuAstClass::NamePartEmptyEdge
        )
    }

    /// Whether the node renders as a bare class name with no constructor call at
    /// all (e.g. `RakuAST::Parameter::Slurpy::Flattened`), unlike the usual
    /// `Class.new(...)` / `Class.new` forms.
    pub fn renders_bare(self) -> bool {
        matches!(
            self,
            RakuAstClass::ParameterSlurpyFlattened
                | RakuAstClass::ParameterSlurpyUnflattened
                | RakuAstClass::ParameterSlurpySingleArgument
                | RakuAstClass::ParameterSlurpyCapture
                | RakuAstClass::RegexBacktrackFrugal
                | RakuAstClass::RegexBacktrackGreedy
                | RakuAstClass::RegexBacktrackRatchet
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

    /// A variable named by one spelling string (`$x` or `$*x`): what a
    /// colonpair value or a regex interpolation's `var` may hold.
    pub fn is_simple_variable(self) -> bool {
        matches!(self, RakuAstClass::VarLexical | RakuAstClass::VarDynamic)
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
            | NumLiteral
            | RatLiteral
            | VersionLiteral
            | ComplexLiteral
            | StrLiteral
            | QuotedString
            | QuotedRegex
            | TypeEnum
            | VarLexical
            | VarPackage
            | VarDynamic
            | TermReduce
            | Sub
            | Block
            | PointyBlock
            | VarDeclarationPlaceholderPositional
            | VarDeclarationPlaceholderNamed
            | VarDeclarationPlaceholderSlurpyArray
            | VarDeclarationPlaceholderSlurpyHash
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
            // Measured MRO: `Fail, Stub, Term, Termish, Expression, ..., Node`.
            StubFail | StubDie | StubWarn => &["RakuAST::Stub", "RakuAST::Term", "RakuAST::Expression"],
            ApplyInfix | ApplyPrefix | ApplyPostfix | ApplyListInfix | ApplyDottyInfix | Ternary => {
                EXPR
            }
            RegexLiteral
            | RegexQuote
            | RegexGroup
            | RegexCapturingGroup
            | RegexNamedCapture
            | RegexAssertionNamed
            | RegexAssertionNamedArgs
            | RegexAssertionAlias
            | RegexAssertionNamedRegexArg
            | RegexAssertionLookahead
            | RegexAssertionInterpolatedVar
            | RegexAssertionCallable
            | RegexAssertionPredicateBlock
            | RegexAssertionInterpolatedBlock
            | RegexInterpolation
            | RegexWithWhitespace
            | RegexBlock => &[
                "RakuAST::Regex::Atom",
                "RakuAST::Regex::Term",
                "RakuAST::Regex",
            ],
            RegexSequence | RegexAlternation | RegexSequentialAlternation => {
                &["RakuAST::Regex"]
            }
            RegexQuantifiedAtom => &["RakuAST::Regex::Term", "RakuAST::Regex"],
            OnlyStar => &[
                "RakuAST::Blockoid",
                "RakuAST::Term",
                "RakuAST::Termish",
                "RakuAST::Expression",
            ],
            RegexMatchFrom | RegexMatchTo | RegexNested | RegexStatement => {
                &["RakuAST::Regex::Atom", "RakuAST::Regex::Term", "RakuAST::Regex"]
            }
            RegexBackReferenceNamed | RegexBackReferencePositional => &[
                "RakuAST::Regex::BackReference",
                "RakuAST::Regex::Atom",
                "RakuAST::Regex::Term",
                "RakuAST::Regex",
            ],
            RegexAssertionPass | RegexAssertionFail | RegexAssertionRecurse => &[
                "RakuAST::Regex::Assertion",
                "RakuAST::Regex::Atom",
                "RakuAST::Regex::Term",
                "RakuAST::Regex",
            ],
            RegexCharClass(kind) => regex_char_class::ancestors(kind),
            RegexAssertionCharClass => &[
                "RakuAST::Regex::Assertion",
                "RakuAST::Regex::Atom",
                "RakuAST::Regex::Term",
                "RakuAST::Regex",
            ],
            RegexCharClassElementEnumeration
            | RegexCharClassElementRule
            | RegexCharClassElementProperty => {
                &["RakuAST::Regex::CharClassElement"]
            }
            RegexInternalModifierIgnoreCase
            | RegexInternalModifierIgnoreMark
            | RegexInternalModifierSigspace
            | RegexInternalModifierRatchet => &[
                "RakuAST::Regex::InternalModifier",
                "RakuAST::Regex::Atom",
                "RakuAST::Regex::Term",
                "RakuAST::Regex",
            ],
            RegexAnchorBeginningOfString
            | RegexAnchorBeginningOfLine
            | RegexAnchorEndOfString
            | RegexAnchorEndOfLine
            | RegexAnchorLeftWordBoundary
            | RegexAnchorRightWordBoundary => &[
                "RakuAST::Regex::Anchor",
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
            ColonPairTrue | ColonPairFalse | ColonPairValue | ColonPairNumber => {
                &["RakuAST::Term", "RakuAST::Expression"]
            }
            Substitution | Transliteration => &[
                "RakuAST::QuotedMatchConstruct",
                "RakuAST::Term",
                "RakuAST::Termish",
                "RakuAST::Expression",
            ],
            StatementPrefixReact | StatementPrefixSupply => &[
                "RakuAST::StatementPrefix::Wheneverable",
                "RakuAST::StatementPrefix::Blorst",
                "RakuAST::StatementPrefix",
                "RakuAST::Term",
                "RakuAST::Termish",
                "RakuAST::Expression",
            ],
            Pragma
            | StatementUse
            | StatementNeed
            | StatementTrusts
            | StatementImport
            | StatementLanguageVersion
            | StatementAlso
            | StatementWhenever => {
                &["RakuAST::Statement"]
            }
            RegexQuantifierZeroOrMore
            | RegexQuantifierOneOrMore
            | RegexQuantifierZeroOrOne
            | RegexQuantifierRange => &["RakuAST::Regex::Quantifier"],
            RegexBacktrackFrugal | RegexBacktrackGreedy | RegexBacktrackRatchet => {
                &["RakuAST::Regex::Backtrack"]
            }
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
    // A `Name::Part` is not a `RakuAST::Node` in Rakudo (its MRO is
    // `(Simple) (Part) (Any) (Mu)`), nor a `RakuAST::Name` despite the
    // `RakuAST::Name::` namespace prefix.
    let is_name_part =
        actual == "RakuAST::Name::Part" || actual.starts_with("RakuAST::Name::Part::");
    if actual == expected || (expected == "RakuAST::Node" && !is_name_part) {
        return true;
    }
    if is_name_part && expected == "RakuAST::Name" {
        return false;
    }
    if crate::qualified::is_inside_package(
        crate::symbol::Symbol::intern(actual),
        crate::symbol::Symbol::intern(expected),
    ) {
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
    let is_name_part =
        class_name == "RakuAST::Name::Part" || class_name.starts_with("RakuAST::Name::Part::");
    let mut namespace = crate::symbol::Symbol::intern(class_name);
    while let Some(parent) = crate::qualified::package_parent(namespace) {
        let parent_sym = parent;
        let parent = parent_sym.as_str();
        if parent == "RakuAST" {
            break;
        }
        if is_name_part && parent == "RakuAST::Name" {
            break;
        }
        if is_registered_type_object(parent) && !mro.iter().any(|name| name == parent) {
            mro.push(parent.to_string());
        }
        namespace = parent_sym;
    }
    for ancestor in semantic_type_object_ancestors(class_name) {
        if !mro.iter().any(|name| name == ancestor) {
            mro.push((*ancestor).to_string());
        }
    }
    if class_name == "RakuAST::Term" && !mro.iter().any(|name| name == "RakuAST::Expression") {
        mro.push("RakuAST::Expression".to_string());
    }
    if class_name != "RakuAST::Node" && !is_name_part {
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
        | "RakuAST::NumLiteral"
        | "RakuAST::RatLiteral"
        | "RakuAST::VersionLiteral"
        | "RakuAST::ComplexLiteral"
        | "RakuAST::StrLiteral"
        | "RakuAST::QuotedString"
        | "RakuAST::Type::Enum"
        | "RakuAST::Var::Lexical"
        | "RakuAST::Var::Package"
        | "RakuAST::Var::Dynamic"
        | "RakuAST::Term::Reduce"
        | "RakuAST::Sub"
        | "RakuAST::Block"
        | "RakuAST::PointyBlock"
        | "RakuAST::VarDeclaration::Placeholder::Positional"
        | "RakuAST::VarDeclaration::Placeholder::Named"
        | "RakuAST::VarDeclaration::Placeholder::SlurpyArray"
        | "RakuAST::VarDeclaration::Placeholder::SlurpyHash"
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
        | "RakuAST::ApplyDottyInfix"
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
        | "RakuAST::Regex::Assertion::Named"
        | "RakuAST::Regex::Assertion::Named::Args"
        | "RakuAST::Regex::Assertion::Alias"
        | "RakuAST::Regex::Assertion::Named::RegexArg"
        | "RakuAST::Regex::Assertion::Lookahead"
        | "RakuAST::Regex::Assertion::InterpolatedVar"
        | "RakuAST::Regex::Assertion::Callable"
        | "RakuAST::Regex::Assertion::PredicateBlock"
        | "RakuAST::Regex::Assertion::InterpolatedBlock"
        | "RakuAST::Regex::Interpolation"
        | "RakuAST::Regex::WithWhitespace"
        | "RakuAST::Regex::Nested"
        | "RakuAST::Regex::Statement"
        | "RakuAST::Regex::Block" => &[
            "RakuAST::Regex::Atom",
            "RakuAST::Regex::Term",
            "RakuAST::Regex",
        ],
        "RakuAST::Regex::BackReference::Named" | "RakuAST::Regex::BackReference::Positional" => &[
            "RakuAST::Regex::BackReference",
            "RakuAST::Regex::Atom",
            "RakuAST::Regex::Term",
            "RakuAST::Regex",
        ],
        "RakuAST::Regex::Assertion::Recurse" => &[
            "RakuAST::Regex::Assertion",
            "RakuAST::Regex::Atom",
            "RakuAST::Regex::Term",
            "RakuAST::Regex",
        ],
        "RakuAST::Regex::Sequence"
        | "RakuAST::Regex::Alternation"
        | "RakuAST::Regex::SequentialAlternation" => {
            &["RakuAST::Regex"]
        },
        "RakuAST::Regex::QuantifiedAtom" =>
            &["RakuAST::Regex::Term", "RakuAST::Regex"],
        "RakuAST::Regex::InternalModifier" => &[
            "RakuAST::Regex::Atom",
            "RakuAST::Regex::Term",
            "RakuAST::Regex",
        ],
        "RakuAST::ColonPair::True"
        | "RakuAST::ColonPair::False"
        | "RakuAST::ColonPair::Variable"
        | "RakuAST::ColonPair::Value"
        | "RakuAST::ColonPair::Number" => {
            &["RakuAST::Term", "RakuAST::Expression"]
        }
        "RakuAST::Substitution" | "RakuAST::Transliteration" => &[
            "RakuAST::QuotedMatchConstruct",
            "RakuAST::Term",
            "RakuAST::Termish",
            "RakuAST::Expression",
        ],
        "RakuAST::Pragma"
        | "RakuAST::Statement::Use"
        | "RakuAST::Statement::LanguageVersion"
        | "RakuAST::Statement::Also" => &["RakuAST::Statement"],
        "RakuAST::RegexDeclaration"
        | "RakuAST::TokenDeclaration"
        | "RakuAST::RuleDeclaration" => &[
            "RakuAST::Term",
            "RakuAST::Expression",
        ],
        // A node class answers what its instances do.
        _ => class_from_name(class_name).map_or(&[], RakuAstClass::semantic_ancestors),
    }
}

/// Whether `class_name` names a RakuAST type object mutsu models (a node
/// class or one of its abstract ancestors).
pub(crate) fn is_registered_type_object(class_name: &str) -> bool {
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
            | "RakuAST::Name::Part"
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
            | "RakuAST::Regex::Assertion"
            | "RakuAST::Regex::Quantifier"
            | "RakuAST::Regex::CharClass"
            | "RakuAST::Regex::CharClass::Negatable"
            | "RakuAST::Regex::CharClassElement"
            | "RakuAST::Regex::Anchor"
            | "RakuAST::Regex::Backtrack"
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
    RakuAstClass::CompUnit,
    RakuAstClass::StatementList,
    RakuAstClass::StatementExpression,
    RakuAstClass::StatementAlso,
    RakuAstClass::StatementPrefixReact,
    RakuAstClass::StatementPrefixSupply,
    RakuAstClass::StatementWhenever,
    RakuAstClass::OnlyStar,
    RakuAstClass::IntLiteral,
    RakuAstClass::NumLiteral,
    RakuAstClass::RatLiteral,
    RakuAstClass::VersionLiteral,
    RakuAstClass::ComplexLiteral,
    RakuAstClass::StrLiteral,
    RakuAstClass::QuotedString,
    RakuAstClass::QuotedRegex,
    RakuAstClass::RegexSequence,
    RakuAstClass::RegexLiteral,
    RakuAstClass::RegexQuote,
    RakuAstClass::RegexWithWhitespace,
    RakuAstClass::RegexBlock,
    RakuAstClass::RegexGroup,
    RakuAstClass::RegexCapturingGroup,
    RakuAstClass::RegexNamedCapture,
    RakuAstClass::RegexAssertionNamed,
    RakuAstClass::RegexAssertionNamedArgs,
    RakuAstClass::RegexAssertionAlias,
    RakuAstClass::RegexAssertionNamedRegexArg,
    RakuAstClass::RegexAssertionLookahead,
    RakuAstClass::RegexAssertionInterpolatedVar,
    RakuAstClass::RegexAssertionCallable,
    RakuAstClass::RegexAssertionPredicateBlock,
    RakuAstClass::RegexAssertionInterpolatedBlock,
    RakuAstClass::RegexInterpolation,
    RakuAstClass::RegexAlternation,
    RakuAstClass::RegexSequentialAlternation,
    RakuAstClass::RegexQuantifiedAtom,
    RakuAstClass::RegexQuantifierZeroOrMore,
    RakuAstClass::RegexQuantifierOneOrMore,
    RakuAstClass::RegexQuantifierZeroOrOne,
    RakuAstClass::RegexAnchorBeginningOfString,
    RakuAstClass::RegexAnchorBeginningOfLine,
    RakuAstClass::RegexAnchorEndOfString,
    RakuAstClass::RegexAnchorEndOfLine,
    RakuAstClass::RegexAnchorLeftWordBoundary,
    RakuAstClass::RegexAnchorRightWordBoundary,
    RakuAstClass::RegexMatchFrom,
    RakuAstClass::RegexAssertionPass,
    RakuAstClass::RegexAssertionFail,
    RakuAstClass::RegexMatchTo,
    RakuAstClass::RegexQuantifierRange,
    RakuAstClass::RegexBacktrackFrugal,
    RakuAstClass::RegexBacktrackGreedy,
    RakuAstClass::RegexBacktrackRatchet,
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::Digit),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::Word),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::Space),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::Newline),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::HorizontalSpace),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::VerticalSpace),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::Tab),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::Escape),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::FormFeed),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::CarriageReturn),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::Nul),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::Any),
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::Specified),
    RakuAstClass::RegexAssertionCharClass,
    RakuAstClass::RegexCharClassElementEnumeration,
    RakuAstClass::RegexCharClassElementRule,
    RakuAstClass::RegexCharClassElementProperty,
    RakuAstClass::RegexCharClassEnumerationElementCharacter,
    RakuAstClass::RegexCharClassEnumerationElementRange,
    RakuAstClass::RegexInternalModifierIgnoreCase,
    RakuAstClass::RegexInternalModifierIgnoreMark,
    RakuAstClass::RegexInternalModifierSigspace,
    RakuAstClass::RegexInternalModifierRatchet,
    RakuAstClass::ColonPairTrue,
    RakuAstClass::ColonPairFalse,
    RakuAstClass::ColonPairVariable,
    RakuAstClass::ColonPairValue,
    RakuAstClass::RegexDeclaration,
    RakuAstClass::TokenDeclaration,
    RakuAstClass::RuleDeclaration,
    RakuAstClass::Grammar,
    RakuAstClass::CallName,
    RakuAstClass::CallNameWithoutParentheses,
    RakuAstClass::Name,
    RakuAstClass::NamePartSimple,
    RakuAstClass::NamePartExpression,
    RakuAstClass::NamePartEmpty,
    RakuAstClass::NamePartEmptyEdge,
    RakuAstClass::ArgList,
    RakuAstClass::VarLexical,
    RakuAstClass::VarPackage,
    RakuAstClass::VarDynamic,
    RakuAstClass::VarDeclarationSimple,
    RakuAstClass::InitializerAssign,
    RakuAstClass::InitializerCallAssign,
    RakuAstClass::VarDeclarationSignature,
    RakuAstClass::InitializerBind,
    RakuAstClass::ApplyInfix,
    RakuAstClass::Infix,
    RakuAstClass::FunctionInfix,
    RakuAstClass::ApplyPrefix,
    RakuAstClass::Prefix,
    RakuAstClass::ApplyPostfix,
    RakuAstClass::Postfix,
    RakuAstClass::Assignment,
    RakuAstClass::MetaInfixAssign,
    RakuAstClass::RegexAssertionRecurse,
    RakuAstClass::RegexBackReferencePositional,
    RakuAstClass::RegexBackReferenceNamed,
    RakuAstClass::RegexStatement,
    RakuAstClass::RegexNested,
    RakuAstClass::VarPositionalCapture,
    RakuAstClass::ColonPairNumber,
    RakuAstClass::Transliteration,
    RakuAstClass::Substitution,
    RakuAstClass::VarNamedCapture,
    RakuAstClass::CallNameAsMethod,
    RakuAstClass::CallTermAsMethod,
    RakuAstClass::FlipFlop,
    RakuAstClass::Feed,
    RakuAstClass::StatementPrefixEager,
    RakuAstClass::TermCapture,
    RakuAstClass::MetaInfixReverse,
    RakuAstClass::MetaInfixCross,
    RakuAstClass::MetaInfixZip,
    RakuAstClass::CallMethod,
    RakuAstClass::CallQuotedMethod,
    RakuAstClass::MetaPostfixHyper,
    RakuAstClass::MetaInfixHyper,
    RakuAstClass::Block,
    RakuAstClass::Blockoid,
    RakuAstClass::PointyBlock,
    RakuAstClass::VarDeclarationPlaceholderPositional,
    RakuAstClass::VarDeclarationPlaceholderNamed,
    RakuAstClass::VarDeclarationPlaceholderSlurpyArray,
    RakuAstClass::VarDeclarationPlaceholderSlurpyHash,
    RakuAstClass::Signature,
    RakuAstClass::Parameter,
    RakuAstClass::ParameterTargetVar,
    RakuAstClass::ParameterTargetTerm,
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
    RakuAstClass::ApplyDottyInfix,
    RakuAstClass::DottyInfixCallAssign,
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
    RakuAstClass::StatementModifierFor,
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
    RakuAstClass::PostcircumfixHashIndex,
    RakuAstClass::PostcircumfixLiteralHashIndex,
    RakuAstClass::TermReduce,
    RakuAstClass::TermEnum,
    RakuAstClass::CircumfixParentheses,
    RakuAstClass::ParameterSlurpyFlattened,
    RakuAstClass::ParameterSlurpyUnflattened,
    RakuAstClass::ParameterSlurpySingleArgument,
    RakuAstClass::ParameterSlurpyCapture,
    RakuAstClass::CircumfixArrayComposer,
    RakuAstClass::CircumfixHashComposer,
    RakuAstClass::ContextualizerHash,
    RakuAstClass::ContextualizerItem,
    RakuAstClass::ContextualizerList,
    RakuAstClass::StatementSequence,
    RakuAstClass::TermWhatever,
    RakuAstClass::WhateverCodeArgument,
    RakuAstClass::TermHyperWhatever,
    RakuAstClass::FatArrow,
    RakuAstClass::StatementPrefixDo,
    RakuAstClass::StatementPrefixTry,
    RakuAstClass::StatementPrefixGather,
    RakuAstClass::CallTerm,
    RakuAstClass::VarDeclarationConstant,
    RakuAstClass::VarDeclarationTerm,
    RakuAstClass::VarDeclarationAnonymous,
    RakuAstClass::TermName,
    RakuAstClass::TermNamed,
    RakuAstClass::TermTopicCall,
    RakuAstClass::CallMetaMethod,
    RakuAstClass::StatementLoopUntil,
    RakuAstClass::StatementLoopRepeatUntil,
    RakuAstClass::TraitIs,
    RakuAstClass::TraitHandles,
    RakuAstClass::TraitDoes,
    RakuAstClass::TraitHides,
    RakuAstClass::StatementTrusts,
    RakuAstClass::StatementPrefixOnce,
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
    RakuAstClass::StatementControl,
    RakuAstClass::TypeSubset,
    RakuAstClass::Module,
    RakuAstClass::Package,
    RakuAstClass::Submethod,
    RakuAstClass::TermSelf,
    RakuAstClass::StubFail,
    RakuAstClass::StubDie,
    RakuAstClass::StubWarn,
    RakuAstClass::Pragma,
    RakuAstClass::StatementUse,
    RakuAstClass::StatementNeed,
    RakuAstClass::StatementImport,
    RakuAstClass::StatementLanguageVersion,
];

/// Entry point for `Str.AST`: parse the source, convert, wrap in `Value::RakuAst`.
pub fn str_dot_ast(source: &str) -> Result<Value, RuntimeError> {
    let (stmts, _finish) = crate::parse_dispatch::parse_source(source)?;
    let node = convert::statement_list(&stmts)?;
    Ok(Value::rakuast(Box::new(node)))
}

/// Entry point for `Str.AST(:compunit)`: the parsed `StatementList` wrapped in
/// a `RakuAST::CompUnit`, as rakudo returns it. Rakudo names each compunit
/// with a fresh 40-hex-digit identifier (two parses of the same source get
/// different names); mutsu derives one from the source and a process-wide
/// counter, which keeps that distinctness.
// Cost: O(n), n = source length (the parse dominates).
pub fn str_dot_ast_compunit(source: &str) -> Result<Value, RuntimeError> {
    use std::hash::{Hash, Hasher};
    use std::sync::atomic::{AtomicU64, Ordering};
    static COMPUNIT_SERIAL: AtomicU64 = AtomicU64::new(0);
    let statement_list = str_dot_ast(source)?;
    let serial = COMPUNIT_SERIAL.fetch_add(1, Ordering::Relaxed);
    let mut words = [0u64; 3];
    for (salt, word) in words.iter_mut().enumerate() {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        (salt, serial, source).hash(&mut hasher);
        *word = hasher.finish();
    }
    let name = format!("{:016X}{:016X}{:08X}", words[0], words[1], words[2] as u32);
    Ok(Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::CompUnit,
        fields: vec![
            RakuAstField {
                name: Some("statement-list"),
                value: RakuAstFieldValue::Node(statement_list),
            },
            RakuAstField {
                name: Some("comp-unit-name"),
                value: RakuAstFieldValue::Node(Value::str(name)),
            },
        ],
    })))
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
        None,
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
    if method == "new"
        && let Some(node) = subscript_adverb::construct(class_name, args)
    {
        return node.map(Some);
    }
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
    // `RakuAST::Call::Name.new(name => ..., args => ...)` and its method-call
    // sibling `RakuAST::Call::Method`; `args` is optional and, like the read
    // direction, omitted when absent.
    if matches!(
        class_name,
        "RakuAST::Call::Name" | "RakuAST::Call::Name::WithoutParentheses" | "RakuAST::Call::Method"
    ) && method == "new"
    {
        let class = class_from_name(class_name).expect("registered call class");
        let name = named_arg(args, "name")
            .ok_or_else(|| RuntimeError::new(format!("{class_name}.new requires `name`")))?;
        let constructor = format!("{class_name}.new");
        require_rakuast_class(&name, RakuAstClass::Name, &constructor)?;
        let mut fields = vec![RakuAstField {
            name: Some("name"),
            value: RakuAstFieldValue::Node(name),
        }];
        if let Some(arg_list) = named_arg(args, "args") {
            require_rakuast_class(&arg_list, RakuAstClass::ArgList, &constructor)?;
            fields.push(RakuAstField {
                name: Some("args"),
                value: RakuAstFieldValue::Node(arg_list),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class,
            fields,
        }))));
    }
    // `RakuAST::Name.new(*@parts)`: the general constructor, needed for any
    // name with a non-identifier part — the empty edge of `::Foo` / `Foo::`
    // or the expression of `::($x)`. An all-identifier name is the same node
    // `from-identifier-parts` builds; the renderer picks the spelling.
    if class_name == "RakuAST::Name" && method == "new" {
        let parts = args
            .iter()
            .map(|part| {
                if is_name_part(part) {
                    Ok(part.clone())
                } else {
                    Err(RuntimeError::new(
                        "RakuAST::Name.new expects RakuAST::Name::Part arguments",
                    ))
                }
            })
            .collect::<Result<Vec<_>, RuntimeError>>()?;
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::Name,
            fields: vec![RakuAstField {
                name: Some("parts"),
                value: RakuAstFieldValue::List(parts),
            }],
        }))));
    }
    if class_name == "RakuAST::Name" && method == "from-identifier-parts" {
        if args.is_empty() {
            return Err(RuntimeError::new(
                "RakuAST::Name.from-identifier-parts expects at least one argument",
            ));
        }
        let parts = args
            .iter()
            .map(|arg| {
                let ValueView::Str(name) = arg.view() else {
                    return Err(RuntimeError::new(
                        "RakuAST::Name.from-identifier-parts expects string arguments",
                    ));
                };
                Ok(Value::rakuast(Box::new(RakuAstNode {
                    class: RakuAstClass::NamePartSimple,
                    fields: vec![RakuAstField {
                        name: None,
                        value: RakuAstFieldValue::Node(Value::str(name.to_string())),
                    }],
                })))
            })
            .collect::<Result<Vec<_>, RuntimeError>>()?;
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::Name,
            fields: vec![RakuAstField {
                name: Some("parts"),
                value: RakuAstFieldValue::List(parts),
            }],
        }))));
    }
    if class_name == "RakuAST::StatementList" && method == "new" {
        for arg in args {
            require_any_rakuast(arg, "RakuAST::StatementList.new", "statements")?;
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::StatementList,
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
    if class_name == "RakuAST::ArgList" && method == "new" {
        for arg in args {
            require_any_rakuast(arg, "RakuAST::ArgList.new", "arguments")?;
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::ArgList,
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
    if class_name == "RakuAST::SemiList" && method == "new" {
        for arg in args {
            require_any_rakuast(arg, "RakuAST::SemiList.new", "arguments")?;
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::SemiList,
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
        if let Some(invocant) = named_arg(args, "invocant") {
            if !matches!(invocant.view(), ValueView::Bool(_)) {
                return Err(RuntimeError::new(
                    "RakuAST::Parameter.new expects `invocant` to be Bool",
                ));
            }
            if invocant.truthy() {
                fields.push(RakuAstField {
                    name: Some("invocant"),
                    value: RakuAstFieldValue::Node(invocant),
                });
            }
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
        let mut fields = vec![RakuAstField {
            name: Some("sigil"),
            value: RakuAstFieldValue::Node(sigil),
        }];
        // Only the dynamic twigil is modelled for a hand-built declaration;
        // the attribute twigils (`.`/`!`) come with a `has` scope this
        // constructor does not take.
        if let Some(twigil) = named_arg(args, "twigil")
            && twigil.truthy()
        {
            if twigil.to_string_value() != "*" {
                return Err(RuntimeError::new(format!(
                    "RakuAST::VarDeclaration::Simple.new does not support twigil '{}'",
                    twigil.to_string_value()
                )));
            }
            fields.push(RakuAstField {
                name: Some("twigil"),
                value: RakuAstFieldValue::Node(twigil),
            });
        }
        fields.push(RakuAstField {
            name: Some("desigilname"),
            value: RakuAstFieldValue::Node(desigilname),
        });
        if let Some(initializer) = named_arg(args, "initializer") {
            let is_initializer = matches!(
                initializer.view(),
                ValueView::RakuAst(n) if matches!(
                    n.class,
                    RakuAstClass::InitializerAssign
                        | RakuAstClass::InitializerBind
                        | RakuAstClass::InitializerCallAssign
                )
            );
            if !is_initializer {
                return Err(RuntimeError::new(
                    "RakuAST::VarDeclaration::Simple.new expects a RakuAST::Initializer node",
                ));
            }
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
        "RakuAST::Regex::Sequence"
            | "RakuAST::Regex::Alternation"
            | "RakuAST::Regex::SequentialAlternation"
    ) && method == "new"
    {
        let class = if class_name.ends_with("Sequence") {
            RakuAstClass::RegexSequence
        } else if class_name.ends_with("SequentialAlternation") {
            RakuAstClass::RegexSequentialAlternation
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
    if let Some(node) = regex_quantifier::construct(class_name, method, args) {
        return node.map(Some);
    }
    if class_name == "RakuAST::Regex::Interpolation" && method == "new" {
        let var = named_arg(args, "var")
            .ok_or_else(|| RuntimeError::new("RakuAST::Regex::Interpolation.new requires `var`"))?;
        require_simple_variable(&var, "RakuAST::Regex::Interpolation.new")?;
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
    if class_name == "RakuAST::Regex::Assertion::Named" && method == "new" {
        let name = named_arg(args, "name").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::Named.new requires `name`")
        })?;
        require_rakuast_class(
            &name,
            RakuAstClass::Name,
            "RakuAST::Regex::Assertion::Named.new",
        )?;
        let capturing = named_arg(args, "capturing").unwrap_or_else(|| Value::truth(false));
        if !matches!(capturing.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::Assertion::Named.new expects `capturing` to be Bool",
            ));
        }
        let mut fields = vec![RakuAstField {
            name: Some("name"),
            value: RakuAstFieldValue::Node(name),
        }];
        if matches!(capturing.view(), ValueView::Bool(true)) {
            fields.push(RakuAstField {
                name: Some("capturing"),
                value: RakuAstFieldValue::Node(capturing),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexAssertionNamed,
            fields,
        }))));
    }
    if class_name == "RakuAST::Regex::Assertion::Named::Args" && method == "new" {
        let name = named_arg(args, "name").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::Named::Args.new requires `name`")
        })?;
        require_rakuast_class(
            &name,
            RakuAstClass::Name,
            "RakuAST::Regex::Assertion::Named::Args.new",
        )?;
        let args_node = named_arg(args, "args").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::Named::Args.new requires `args`")
        })?;
        require_rakuast_class(
            &args_node,
            RakuAstClass::ArgList,
            "RakuAST::Regex::Assertion::Named::Args.new",
        )?;
        let capturing = named_arg(args, "capturing").unwrap_or_else(|| Value::truth(false));
        if !matches!(capturing.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::Assertion::Named::Args.new expects `capturing` to be Bool",
            ));
        }
        let mut fields = vec![RakuAstField {
            name: Some("name"),
            value: RakuAstFieldValue::Node(name),
        }];
        if let ValueView::RakuAst(args_ast) = args_node.view()
            && !args_ast.fields.is_empty()
        {
            fields.push(RakuAstField {
                name: Some("args"),
                value: RakuAstFieldValue::Node(args_node),
            });
        }
        if matches!(capturing.view(), ValueView::Bool(true)) {
            fields.push(RakuAstField {
                name: Some("capturing"),
                value: RakuAstFieldValue::Node(capturing),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexAssertionNamedArgs,
            fields,
        }))));
    }
    if class_name == "RakuAST::ColonPair::Variable" && method == "new" {
        let key = named_arg(args, "key")
            .ok_or_else(|| RuntimeError::new("RakuAST::ColonPair::Variable.new requires `key`"))?;
        let ValueView::Str(key) = key.view() else {
            return Err(RuntimeError::new(
                "RakuAST::ColonPair::Variable.new expects `key` to be a Str",
            ));
        };
        let value = named_arg(args, "value").ok_or_else(|| {
            RuntimeError::new("RakuAST::ColonPair::Variable.new requires `value`")
        })?;
        require_simple_variable(&value, "RakuAST::ColonPair::Variable.new")?;
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::ColonPairVariable,
            fields: vec![
                RakuAstField {
                    name: Some("key"),
                    value: RakuAstFieldValue::Node(Value::str(key.to_string())),
                },
                RakuAstField {
                    name: Some("value"),
                    value: RakuAstFieldValue::Node(value),
                },
            ],
        }))));
    }
    if class_name == "RakuAST::ColonPair::Value" && method == "new" {
        let key = named_arg(args, "key")
            .ok_or_else(|| RuntimeError::new("RakuAST::ColonPair::Value.new requires `key`"))?;
        let ValueView::Str(key) = key.view() else {
            return Err(RuntimeError::new(
                "RakuAST::ColonPair::Value.new expects `key` to be a Str",
            ));
        };
        let value = named_arg(args, "value")
            .ok_or_else(|| RuntimeError::new("RakuAST::ColonPair::Value.new requires `value`"))?;
        require_any_rakuast(&value, "RakuAST::ColonPair::Value.new", "value")?;
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::ColonPairValue,
            fields: vec![
                RakuAstField {
                    name: Some("key"),
                    value: RakuAstFieldValue::Node(Value::str(key.to_string())),
                },
                RakuAstField {
                    name: Some("value"),
                    value: RakuAstFieldValue::Node(value),
                },
            ],
        }))));
    }
    if class_name == "RakuAST::Regex::Assertion::Alias" && method == "new" {
        let name = named_arg(args, "name").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::Alias.new requires `name`")
        })?;
        if !matches!(name.view(), ValueView::Str(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::Assertion::Alias.new expects `name` to be Str",
            ));
        }
        let assertion = named_arg(args, "assertion").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::Alias.new requires `assertion`")
        })?;
        match assertion.view() {
            ValueView::RakuAst(node)
                if matches!(
                    node.class,
                    RakuAstClass::RegexAssertionNamed | RakuAstClass::RegexAssertionNamedArgs
                ) => {}
            _ => {
                return Err(RuntimeError::new(
                    "RakuAST::Regex::Assertion::Alias.new expects `assertion` to be a named regex assertion",
                ));
            }
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexAssertionAlias,
            fields: vec![
                RakuAstField {
                    name: Some("name"),
                    value: RakuAstFieldValue::Node(name),
                },
                RakuAstField {
                    name: Some("assertion"),
                    value: RakuAstFieldValue::Node(assertion),
                },
            ],
        }))));
    }
    if class_name == "RakuAST::Regex::Assertion::Named::RegexArg" && method == "new" {
        let name = named_arg(args, "name").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::Named::RegexArg.new requires `name`")
        })?;
        require_rakuast_class(
            &name,
            RakuAstClass::Name,
            "RakuAST::Regex::Assertion::Named::RegexArg.new",
        )?;
        let regex_arg = named_arg(args, "regex-arg").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::Named::RegexArg.new requires `regex-arg`")
        })?;
        require_regex_node(&regex_arg, "RakuAST::Regex::Assertion::Named::RegexArg.new")?;
        let capturing = named_arg(args, "capturing").unwrap_or_else(|| Value::truth(true));
        if !matches!(capturing.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::Assertion::Named::RegexArg.new expects `capturing` to be Bool",
            ));
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexAssertionNamedRegexArg,
            fields: vec![
                RakuAstField {
                    name: Some("name"),
                    value: RakuAstFieldValue::Node(name),
                },
                RakuAstField {
                    name: Some("regex-arg"),
                    value: RakuAstFieldValue::Node(regex_arg),
                },
                RakuAstField {
                    name: Some("capturing"),
                    value: RakuAstFieldValue::Node(capturing),
                },
            ],
        }))));
    }
    if class_name == "RakuAST::Regex::Assertion::Lookahead" && method == "new" {
        let assertion = named_arg(args, "assertion").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::Lookahead.new requires `assertion`")
        })?;
        if !matches!(
            assertion.view(),
            ValueView::RakuAst(node)
                if matches!(
                    node.class,
                    RakuAstClass::RegexAssertionNamedRegexArg
                        | RakuAstClass::RegexAssertionInterpolatedVar
                )
        ) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::Assertion::Lookahead.new expects a supported assertion node",
            ));
        }
        let negated = named_arg(args, "negated").unwrap_or_else(|| Value::truth(false));
        if !matches!(negated.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::Assertion::Lookahead.new expects `negated` to be Bool",
            ));
        }
        let mut fields = Vec::new();
        if matches!(negated.view(), ValueView::Bool(true)) {
            fields.push(RakuAstField {
                name: Some("negated"),
                value: RakuAstFieldValue::Node(negated),
            });
        }
        fields.push(RakuAstField {
            name: Some("assertion"),
            value: RakuAstFieldValue::Node(assertion),
        });
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexAssertionLookahead,
            fields,
        }))));
    }
    if class_name == "RakuAST::Regex::Assertion::InterpolatedVar" && method == "new" {
        let var = named_arg(args, "var").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::InterpolatedVar.new requires `var`")
        })?;
        require_simple_variable(&var, "RakuAST::Regex::Assertion::InterpolatedVar.new")?;
        let sequential = named_arg(args, "sequential").unwrap_or_else(|| Value::truth(false));
        if !matches!(sequential.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::Assertion::InterpolatedVar.new expects `sequential` to be Bool",
            ));
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexAssertionInterpolatedVar,
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
    if class_name == "RakuAST::Regex::Assertion::Callable" && method == "new" {
        let callee = named_arg(args, "callee").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::Callable.new requires `callee`")
        })?;
        require_simple_variable(&callee, "RakuAST::Regex::Assertion::Callable.new")?;
        let args_node = named_arg(args, "args");
        if let Some(args_node) = &args_node {
            require_rakuast_class(
                args_node,
                RakuAstClass::ArgList,
                "RakuAST::Regex::Assertion::Callable.new",
            )?;
        }
        let mut fields = vec![RakuAstField {
            name: Some("callee"),
            value: RakuAstFieldValue::Node(callee),
        }];
        // Rakudo omits an empty ArgList from the rendered node even when the
        // caller supplied `args => ArgList.new`.
        if let Some(args_node) = args_node
            && let ValueView::RakuAst(node) = args_node.view()
            && !node.fields.is_empty()
        {
            fields.push(RakuAstField {
                name: Some("args"),
                value: RakuAstFieldValue::Node(args_node),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexAssertionCallable,
            fields,
        }))));
    }
    if class_name == "RakuAST::Regex::Assertion::PredicateBlock" && method == "new" {
        let block = named_arg(args, "block").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::PredicateBlock.new requires block")
        })?;
        require_rakuast_class(
            &block,
            RakuAstClass::Block,
            "RakuAST::Regex::Assertion::PredicateBlock.new",
        )?;
        let negated = named_arg(args, "negated").unwrap_or_else(|| Value::truth(false));
        if !matches!(negated.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::Assertion::PredicateBlock.new expects negated to be Bool",
            ));
        }
        let mut fields = Vec::new();
        if matches!(negated.view(), ValueView::Bool(true)) {
            fields.push(RakuAstField {
                name: Some("negated"),
                value: RakuAstFieldValue::Node(negated),
            });
        }
        fields.push(RakuAstField {
            name: Some("block"),
            value: RakuAstFieldValue::Node(block),
        });
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexAssertionPredicateBlock,
            fields,
        }))));
    }
    if class_name == "RakuAST::Regex::Assertion::InterpolatedBlock" && method == "new" {
        let block = named_arg(args, "block").ok_or_else(|| {
            RuntimeError::new("RakuAST::Regex::Assertion::InterpolatedBlock.new requires block")
        })?;
        require_rakuast_class(
            &block,
            RakuAstClass::Block,
            "RakuAST::Regex::Assertion::InterpolatedBlock.new",
        )?;
        let sequential = named_arg(args, "sequential").unwrap_or_else(|| Value::truth(false));
        if !matches!(sequential.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::Assertion::InterpolatedBlock.new expects sequential to be Bool",
            ));
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::RegexAssertionInterpolatedBlock,
            fields: vec![
                RakuAstField {
                    name: Some("block"),
                    value: RakuAstFieldValue::Node(block),
                },
                RakuAstField {
                    name: Some("sequential"),
                    value: RakuAstFieldValue::Node(sequential),
                },
            ],
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
    if let Some(node) = regex_char_class::construct(class_name, method, args) {
        return node.map(Some);
    }
    if let Some(node) = regex_enumeration::construct(class_name, method, args) {
        return node.map(Some);
    }
    if let Some(node) = role::construct(class_name, method, args) {
        return node.map(Some);
    }
    // `Regex::InternalModifier::IgnoreCase.new(:modifier<ignorecase>, :negated)`:
    // both nameds optional. A field equal to its default (the short spelling,
    // `False`) is left off, which is what the renderer then elides.
    let modifier_class = match (class_name, method) {
        ("RakuAST::Regex::InternalModifier::IgnoreCase", "new") => {
            Some((RakuAstClass::RegexInternalModifierIgnoreCase, "i"))
        }
        ("RakuAST::Regex::InternalModifier::IgnoreMark", "new") => {
            Some((RakuAstClass::RegexInternalModifierIgnoreMark, "m"))
        }
        ("RakuAST::Regex::InternalModifier::Sigspace", "new") => {
            Some((RakuAstClass::RegexInternalModifierSigspace, "s"))
        }
        ("RakuAST::Regex::InternalModifier::Ratchet", "new") => {
            Some((RakuAstClass::RegexInternalModifierRatchet, "r"))
        }
        _ => None,
    };
    if let Some((class, short)) = modifier_class {
        let mut fields = Vec::new();
        if let Some(modifier) = named_arg(args, "modifier")
            && modifier.to_string_value() != short
        {
            fields.push(RakuAstField {
                name: Some("modifier"),
                value: RakuAstFieldValue::Node(Value::str(modifier.to_string_value())),
            });
        }
        if named_arg(args, "negated").is_some_and(|v| v.truthy()) {
            fields.push(RakuAstField {
                name: Some("negated"),
                value: RakuAstFieldValue::Node(Value::truth(true)),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            class,
            fields,
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
            let value = match named_arg(args, fname) {
                Some(value) => value,
                // A block's `body` defaults to an empty blockoid in raku:
                // `RakuAST::Block.new` is a complete (empty) block.
                None if fname == "body" && class == RakuAstClass::Block => empty_blockoid(),
                None => {
                    return Err(RuntimeError::new(format!(
                        "{class_name}.{method} requires a `{fname}` argument"
                    )));
                }
            };
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

/// A `Var::Lexical` or `Var::Dynamic` node, as a variable-holding slot expects.
fn require_simple_variable(value: &Value, constructor: &str) -> Result<(), RuntimeError> {
    match value.view() {
        ValueView::RakuAst(node) if node.class.is_simple_variable() => Ok(()),
        _ => Err(RuntimeError::new(format!(
            "{constructor} expects a RakuAST::Var::Lexical or RakuAST::Var::Dynamic node"
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
                    | RakuAstClass::RegexBlock
                    | RakuAstClass::RegexGroup
                    | RakuAstClass::RegexCapturingGroup
                    | RakuAstClass::RegexNamedCapture
                    | RakuAstClass::RegexAssertionNamed
                    | RakuAstClass::RegexAssertionNamedArgs
                    | RakuAstClass::RegexAssertionAlias
                    | RakuAstClass::RegexAssertionNamedRegexArg
                    | RakuAstClass::RegexAssertionLookahead
                    | RakuAstClass::RegexAssertionInterpolatedVar
                    | RakuAstClass::RegexAssertionCallable
                    | RakuAstClass::RegexAssertionPredicateBlock
                    | RakuAstClass::RegexAssertionInterpolatedBlock
                    | RakuAstClass::RegexInterpolation
                    | RakuAstClass::RegexAlternation
                    | RakuAstClass::RegexSequentialAlternation
                    | RakuAstClass::RegexQuantifiedAtom
                    | RakuAstClass::RegexAnchorBeginningOfString
                    | RakuAstClass::RegexAnchorBeginningOfLine
                    | RakuAstClass::RegexAnchorEndOfString
                    | RakuAstClass::RegexAnchorEndOfLine
                    | RakuAstClass::RegexAnchorLeftWordBoundary
                    | RakuAstClass::RegexAnchorRightWordBoundary
                    | RakuAstClass::RegexMatchFrom
                    | RakuAstClass::RegexAssertionPass
                    | RakuAstClass::RegexAssertionFail
                    | RakuAstClass::RegexAssertionRecurse
                    | RakuAstClass::RegexNested
                    | RakuAstClass::RegexStatement
                    | RakuAstClass::RegexBackReferenceNamed
                    | RakuAstClass::RegexBackReferencePositional
                    | RakuAstClass::RegexMatchTo
                    | RakuAstClass::RegexCharClass(_)
                    | RakuAstClass::RegexAssertionCharClass
                    | RakuAstClass::RegexInternalModifierIgnoreCase
                    | RakuAstClass::RegexInternalModifierIgnoreMark
                    | RakuAstClass::RegexInternalModifierSigspace
                    | RakuAstClass::RegexInternalModifierRatchet
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
        ValueView::RakuAst(node) if node.class.renders_bare() => Some(node.class),
        ValueView::Package(name) => match name.resolve().as_str() {
            "RakuAST::Parameter::Slurpy::Flattened" => Some(RakuAstClass::ParameterSlurpyFlattened),
            "RakuAST::Parameter::Slurpy::Unflattened" => {
                Some(RakuAstClass::ParameterSlurpyUnflattened)
            }
            "RakuAST::Parameter::Slurpy::SingleArgument" => {
                Some(RakuAstClass::ParameterSlurpySingleArgument)
            }
            "RakuAST::Parameter::Slurpy::Capture" => Some(RakuAstClass::ParameterSlurpyCapture),
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
        ("RakuAST::NumLiteral", "new") => RakuAstClass::NumLiteral,
        ("RakuAST::RatLiteral", "new") => RakuAstClass::RatLiteral,
        ("RakuAST::VersionLiteral", "new") => RakuAstClass::VersionLiteral,
        ("RakuAST::ComplexLiteral", "new") => RakuAstClass::ComplexLiteral,
        ("RakuAST::StrLiteral", "new") => RakuAstClass::StrLiteral,
        ("RakuAST::Name", "from-identifier") => RakuAstClass::Name,
        ("RakuAST::Trait::Does", "new") => RakuAstClass::TraitDoes,
        ("RakuAST::Trait::Hides", "new") => RakuAstClass::TraitHides,
        ("RakuAST::Name::Part::Simple", "new") => RakuAstClass::NamePartSimple,
        ("RakuAST::Name::Part::Expression", "new") => RakuAstClass::NamePartExpression,
        ("RakuAST::Term::Name", "new") => RakuAstClass::TermName,
        ("RakuAST::Term::Named", "new") => RakuAstClass::TermNamed,
        ("RakuAST::Term::TopicCall", "new") => RakuAstClass::TermTopicCall,
        ("RakuAST::ParameterTarget::Term", "new") => RakuAstClass::ParameterTargetTerm,
        ("RakuAST::Term::Enum", "from-identifier") => RakuAstClass::TermEnum,
        ("RakuAST::Infix", "new") => RakuAstClass::Infix,
        ("RakuAST::FunctionInfix", "new") => RakuAstClass::FunctionInfix,
        ("RakuAST::MetaInfix::Assign", "new") => RakuAstClass::MetaInfixAssign,
        ("RakuAST::Regex::Assertion::Recurse", "new") => RakuAstClass::RegexAssertionRecurse,
        ("RakuAST::Regex::BackReference::Positional", "new") => {
            RakuAstClass::RegexBackReferencePositional
        }
        ("RakuAST::Regex::BackReference::Named", "new") => RakuAstClass::RegexBackReferenceNamed,
        ("RakuAST::Regex::Statement", "new") => RakuAstClass::RegexStatement,
        ("RakuAST::Regex::Nested", "new") => RakuAstClass::RegexNested,
        ("RakuAST::Var::PositionalCapture", "new") => RakuAstClass::VarPositionalCapture,
        ("RakuAST::ColonPair::Number", "new") => RakuAstClass::ColonPairNumber,
        ("RakuAST::Transliteration", "new") => RakuAstClass::Transliteration,
        ("RakuAST::Substitution", "new") => RakuAstClass::Substitution,
        ("RakuAST::Var::NamedCapture", "new") => RakuAstClass::VarNamedCapture,
        ("RakuAST::Call::NameAsMethod", "new") => RakuAstClass::CallNameAsMethod,
        ("RakuAST::Call::TermAsMethod", "new") => RakuAstClass::CallTermAsMethod,
        ("RakuAST::FlipFlop", "new") => RakuAstClass::FlipFlop,
        ("RakuAST::Feed", "new") => RakuAstClass::Feed,
        ("RakuAST::StatementPrefix::Eager", "new") => RakuAstClass::StatementPrefixEager,
        ("RakuAST::Term::Capture", "new") => RakuAstClass::TermCapture,
        ("RakuAST::MetaInfix::Reverse", "new") => RakuAstClass::MetaInfixReverse,
        ("RakuAST::MetaInfix::Cross", "new") => RakuAstClass::MetaInfixCross,
        ("RakuAST::MetaInfix::Zip", "new") => RakuAstClass::MetaInfixZip,
        ("RakuAST::Prefix", "new") => RakuAstClass::Prefix,
        ("RakuAST::Var::Lexical", "new") => RakuAstClass::VarLexical,
        ("RakuAST::Var::Dynamic", "new") => RakuAstClass::VarDynamic,
        ("RakuAST::Circumfix::Parentheses", "new") => RakuAstClass::CircumfixParentheses,
        ("RakuAST::VarDeclaration::Placeholder::Positional", "new") => {
            RakuAstClass::VarDeclarationPlaceholderPositional
        }
        ("RakuAST::VarDeclaration::Placeholder::Named", "new") => {
            RakuAstClass::VarDeclarationPlaceholderNamed
        }
        ("RakuAST::Initializer::Assign", "new") => RakuAstClass::InitializerAssign,
        ("RakuAST::Initializer::CallAssign", "new") => RakuAstClass::InitializerCallAssign,
        ("RakuAST::Initializer::Bind", "new") => RakuAstClass::InitializerBind,
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
        ("RakuAST::Regex::Block", "new") => RakuAstClass::RegexBlock,
        ("RakuAST::ColonPair::True", "new") => RakuAstClass::ColonPairTrue,
        ("RakuAST::ColonPair::False", "new") => RakuAstClass::ColonPairFalse,
        // Each phaser kind wraps its blorst positionally, exactly as the read
        // direction (`convert.rs`) builds it.
        ("RakuAST::StatementPrefix::Phaser::Begin", "new") => {
            RakuAstClass::StatementPrefixPhaserBegin
        }
        ("RakuAST::StatementPrefix::Once", "new") => RakuAstClass::StatementPrefixOnce,
        ("RakuAST::StatementPrefix::React", "new") => RakuAstClass::StatementPrefixReact,
        ("RakuAST::StatementPrefix::Supply", "new") => RakuAstClass::StatementPrefixSupply,
        ("RakuAST::StatementPrefix::Phaser::Check", "new") => {
            RakuAstClass::StatementPrefixPhaserCheck
        }
        ("RakuAST::StatementPrefix::Phaser::Init", "new") => {
            RakuAstClass::StatementPrefixPhaserInit
        }
        ("RakuAST::StatementPrefix::Phaser::End", "new") => RakuAstClass::StatementPrefixPhaserEnd,
        ("RakuAST::StatementPrefix::Phaser::Enter", "new") => {
            RakuAstClass::StatementPrefixPhaserEnter
        }
        ("RakuAST::StatementPrefix::Phaser::Leave", "new") => {
            RakuAstClass::StatementPrefixPhaserLeave
        }
        ("RakuAST::StatementPrefix::Phaser::Keep", "new") => {
            RakuAstClass::StatementPrefixPhaserKeep
        }
        ("RakuAST::StatementPrefix::Phaser::Undo", "new") => {
            RakuAstClass::StatementPrefixPhaserUndo
        }
        ("RakuAST::StatementPrefix::Phaser::First", "new") => {
            RakuAstClass::StatementPrefixPhaserFirst
        }
        ("RakuAST::StatementPrefix::Phaser::Next", "new") => {
            RakuAstClass::StatementPrefixPhaserNext
        }
        ("RakuAST::StatementPrefix::Phaser::Last", "new") => {
            RakuAstClass::StatementPrefixPhaserLast
        }
        ("RakuAST::StatementPrefix::Phaser::Quit", "new") => {
            RakuAstClass::StatementPrefixPhaserQuit
        }
        ("RakuAST::StatementPrefix::Phaser::Close", "new") => {
            RakuAstClass::StatementPrefixPhaserClose
        }
        _ => return None,
    })
}

fn zero_positional_class(class_name: &str, method: &str) -> Option<RakuAstClass> {
    Some(match (class_name, method) {
        ("RakuAST::VarDeclaration::Placeholder::SlurpyArray", "new") => {
            RakuAstClass::VarDeclarationPlaceholderSlurpyArray
        }
        ("RakuAST::VarDeclaration::Placeholder::SlurpyHash", "new") => {
            RakuAstClass::VarDeclarationPlaceholderSlurpyHash
        }
        ("RakuAST::Regex::Anchor::BeginningOfString", "new") => {
            RakuAstClass::RegexAnchorBeginningOfString
        }
        ("RakuAST::Regex::Anchor::BeginningOfLine", "new") => {
            RakuAstClass::RegexAnchorBeginningOfLine
        }
        ("RakuAST::Regex::Anchor::EndOfString", "new") => RakuAstClass::RegexAnchorEndOfString,
        ("RakuAST::Regex::Anchor::EndOfLine", "new") => RakuAstClass::RegexAnchorEndOfLine,
        ("RakuAST::Regex::Anchor::LeftWordBoundary", "new") => {
            RakuAstClass::RegexAnchorLeftWordBoundary
        }
        ("RakuAST::Regex::Anchor::RightWordBoundary", "new") => {
            RakuAstClass::RegexAnchorRightWordBoundary
        }
        ("RakuAST::Regex::MatchFrom", "new") => RakuAstClass::RegexMatchFrom,
        ("RakuAST::Regex::Assertion::Pass", "new") => RakuAstClass::RegexAssertionPass,
        ("RakuAST::Regex::Assertion::Fail", "new") => RakuAstClass::RegexAssertionFail,
        ("RakuAST::OnlyStar", "new") => RakuAstClass::OnlyStar,
        ("RakuAST::Regex::MatchTo", "new") => RakuAstClass::RegexMatchTo,
        ("RakuAST::Term::Whatever", "new") => RakuAstClass::TermWhatever,
        ("RakuAST::Name::Part::Empty", "new") => RakuAstClass::NamePartEmpty,
        ("RakuAST::Name::Part::EmptyEdge", "new") => RakuAstClass::NamePartEmptyEdge,
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
        ("RakuAST::Statement::Whenever", "new") => {
            (RakuAstClass::StatementWhenever, &["trigger", "body"][..])
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
        ("RakuAST::CompUnit", "new") => (
            RakuAstClass::CompUnit,
            &["statement-list", "comp-unit-name"][..],
        ),
        ("RakuAST::PointyBlock", "new") => (RakuAstClass::PointyBlock, &["signature", "body"][..]),
        ("RakuAST::Var::Package", "new") => (RakuAstClass::VarPackage, &["name", "sigil"][..]),
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
/// returns the positional children of a `StatementList`/`SemiList` as a `List`.
pub fn node_accessor(node: &RakuAstNode, method: &str) -> Option<Value> {
    if let Some(child) = regex_extension::nested_accessor(node, method) {
        return Some(child);
    }
    for f in &node.fields {
        if f.name == Some(method) {
            return Some(field_to_value(&f.value));
        }
    }
    if (method == "statements"
        && matches!(
            node.class,
            RakuAstClass::StatementList | RakuAstClass::StatementSequence | RakuAstClass::SemiList
        ))
        || (method == "args" && node.class == RakuAstClass::ArgList)
    {
        let items = node
            .fields
            .iter()
            .map(|f| field_to_value(&f.value))
            .collect();
        return Some(Value::array(items));
    }
    // Positional-leaf accessors: a node whose positional field is its payload
    // exposes it under a class-specific name (`IntLiteral.value`,
    // `Var::Lexical.name`). Regex sequences and alternations are the one
    // variadic case: their accessor returns all positional children as a List.
    if matches!(
        node.class,
        RakuAstClass::RegexSequence
            | RakuAstClass::RegexAlternation
            | RakuAstClass::RegexSequentialAlternation
            | RakuAstClass::RegexAssertionCharClass
    ) && fields::positional_accessor(node.class) == Some(method)
    {
        let items = node
            .fields
            .iter()
            .filter(|f| f.name.is_none())
            .map(|f| field_to_value(&f.value))
            .collect();
        return Some(Value::array(items));
    }
    // The named-field loop above runs first, so a class with a named field of
    // the same name (e.g. `Call::Name.name`) is unaffected.
    if fields::positional_accessor(node.class) == Some(method)
        && let Some(f) = node.fields.first()
        && f.name.is_none()
    {
        return Some(field_to_value(&f.value));
    }
    // `Var::Lexical.sigil` / `Var::Dynamic.sigil`: derived from the spelling.
    if method == "sigil"
        && node.class.is_simple_variable()
        && let Some(f) = node.fields.first()
        && let RakuAstFieldValue::Node(v) = &f.value
        && let ValueView::Str(s) = v.view()
        && let Some(sigil) = s.chars().next()
    {
        return Some(Value::str(sigil.to_string()));
    }
    // `Name.from-identifier("a")` stores one identifier leaf; `.parts` answers
    // the one `Name::Part::Simple` it denotes (rakudo does not split on `::`).
    if method == "parts"
        && node.class == RakuAstClass::Name
        && let Some(RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(v),
        }) = node.fields.first()
        && let ValueView::Str(s) = v.view()
    {
        return Some(Value::array(vec![name_parts::simple_part(&s)]));
    }
    // `Name.canonicalize`: the `::`-joined spelling of a static identifier name.
    if method == "canonicalize"
        && let Some(name_parts::NameShape::Identifier(s)) = name_parts::name_shape(node)
    {
        return Some(Value::str(s));
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
            if class == RakuAstClass::Name {
                names.push("from-identifier-parts");
                names.push("new");
            }
        }
        Constructor::New if constructor_is_supported(class) => names.push("new"),
        _ => {}
    }
    names.extend(accessor_names(class));
    if class == RakuAstClass::CompUnit {
        names.push("replace-statement-list");
    }
    if class == RakuAstClass::StatementExpression {
        names.push("set-expression");
    }
    if class == RakuAstClass::ArgList {
        names.push("push");
    }
    if class == RakuAstClass::StatementList {
        names.push("add-statement");
        names.push("unshift-statement");
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
        RakuAstClass::CompUnit
            | RakuAstClass::RegexInternalModifierIgnoreCase
            | RakuAstClass::RegexInternalModifierIgnoreMark
            | RakuAstClass::RegexInternalModifierSigspace
            | RakuAstClass::RegexInternalModifierRatchet
            | RakuAstClass::ArgList
            | RakuAstClass::SemiList
            | RakuAstClass::StatementList
            | RakuAstClass::IntLiteral
            | RakuAstClass::NumLiteral
            | RakuAstClass::RatLiteral
            | RakuAstClass::VersionLiteral
            | RakuAstClass::ComplexLiteral
            | RakuAstClass::StrLiteral
            | RakuAstClass::Infix
            | RakuAstClass::Prefix
            | RakuAstClass::VarLexical
            | RakuAstClass::VarPackage
            | RakuAstClass::VarDynamic
            | RakuAstClass::StatementExpression
            | RakuAstClass::ApplyInfix
            | RakuAstClass::ApplyPrefix
            | RakuAstClass::ApplyPostfix
            | RakuAstClass::PostcircumfixLiteralHashIndex
            | RakuAstClass::PostcircumfixArrayIndex
            | RakuAstClass::PostcircumfixHashIndex
            | RakuAstClass::FunctionInfix
            | RakuAstClass::Postfix
            | RakuAstClass::MetaInfixAssign
            | RakuAstClass::RegexAssertionRecurse
            | RakuAstClass::RegexBackReferencePositional
            | RakuAstClass::RegexBackReferenceNamed
            | RakuAstClass::RegexStatement
            | RakuAstClass::RegexNested
            | RakuAstClass::VarPositionalCapture
            | RakuAstClass::ColonPairNumber
            | RakuAstClass::Transliteration
            | RakuAstClass::Substitution
            | RakuAstClass::VarNamedCapture
            | RakuAstClass::CallNameAsMethod
            | RakuAstClass::CallTermAsMethod
            | RakuAstClass::FlipFlop
            | RakuAstClass::Feed
            | RakuAstClass::StatementPrefixEager
            | RakuAstClass::TermCapture
            | RakuAstClass::MetaInfixReverse
            | RakuAstClass::MetaInfixCross
            | RakuAstClass::MetaInfixZip
            | RakuAstClass::Block
            | RakuAstClass::PointyBlock
            | RakuAstClass::Blockoid
            | RakuAstClass::CircumfixParentheses
            | RakuAstClass::VarDeclarationPlaceholderPositional
            | RakuAstClass::VarDeclarationPlaceholderNamed
            | RakuAstClass::VarDeclarationPlaceholderSlurpyArray
            | RakuAstClass::VarDeclarationPlaceholderSlurpyHash
            | RakuAstClass::Sub
            | RakuAstClass::Signature
            | RakuAstClass::TraitReturns
            | RakuAstClass::TraitOf
            | RakuAstClass::Parameter
            | RakuAstClass::ParameterTargetVar
            | RakuAstClass::ParameterTargetTerm
            | RakuAstClass::VarDeclarationSimple
            | RakuAstClass::InitializerAssign
            | RakuAstClass::InitializerCallAssign
            | RakuAstClass::InitializerBind
            | RakuAstClass::TypeSimple
            | RakuAstClass::TypeEnum
            | RakuAstClass::TypeSetting
            | RakuAstClass::TypeCapture
            | RakuAstClass::QuotedRegex
            | RakuAstClass::RegexSequence
            | RakuAstClass::RegexAlternation
            | RakuAstClass::RegexSequentialAlternation
            | RakuAstClass::RegexLiteral
            | RakuAstClass::RegexQuote
            | RakuAstClass::RegexWithWhitespace
            | RakuAstClass::RegexBlock
            | RakuAstClass::RegexGroup
            | RakuAstClass::RegexCapturingGroup
            | RakuAstClass::RegexNamedCapture
            | RakuAstClass::RegexAssertionNamed
            | RakuAstClass::RegexAssertionNamedArgs
            | RakuAstClass::RegexAssertionAlias
            | RakuAstClass::RegexAssertionNamedRegexArg
            | RakuAstClass::RegexAssertionLookahead
            | RakuAstClass::RegexAssertionInterpolatedVar
            | RakuAstClass::RegexAssertionCallable
            | RakuAstClass::RegexAssertionPredicateBlock
            | RakuAstClass::RegexAssertionInterpolatedBlock
            | RakuAstClass::RegexInterpolation
            | RakuAstClass::RegexQuantifiedAtom
            | RakuAstClass::RegexQuantifierZeroOrMore
            | RakuAstClass::RegexQuantifierOneOrMore
            | RakuAstClass::RegexQuantifierZeroOrOne
            | RakuAstClass::RegexAnchorBeginningOfString
            | RakuAstClass::RegexAnchorBeginningOfLine
            | RakuAstClass::RegexAnchorEndOfString
            | RakuAstClass::RegexAnchorEndOfLine
            | RakuAstClass::RegexAnchorLeftWordBoundary
            | RakuAstClass::RegexAnchorRightWordBoundary
            | RakuAstClass::RegexMatchFrom
            | RakuAstClass::RegexAssertionPass
            | RakuAstClass::RegexAssertionFail
            | RakuAstClass::RegexMatchTo
            | RakuAstClass::RegexQuantifierRange
            | RakuAstClass::RegexCharClass(_)
            | RakuAstClass::ColonPairTrue
            | RakuAstClass::ColonPairFalse
            | RakuAstClass::ColonPairVariable
            | RakuAstClass::ColonPairValue
            | RakuAstClass::RegexDeclaration
            | RakuAstClass::TokenDeclaration
            | RakuAstClass::RuleDeclaration
            | RakuAstClass::Grammar
            | RakuAstClass::NamePartSimple
            | RakuAstClass::NamePartExpression
            | RakuAstClass::NamePartEmpty
            | RakuAstClass::NamePartEmptyEdge
            | RakuAstClass::TermName
            | RakuAstClass::TermNamed
            | RakuAstClass::TermTopicCall
            | RakuAstClass::TermWhatever
            | RakuAstClass::CallName
            | RakuAstClass::CallNameWithoutParentheses
            | RakuAstClass::CallMethod
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
