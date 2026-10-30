//! RakuAST — a reflection/model layer over the internal AST (ADR-0011).
//!
//! `Str.AST` converts the internal AST into a [`RakuAstNode`] tree, whose
//! rendering exposes RakuAST constructors. EVAL lowers those nodes to the
//! existing compiler input. ADR-10723 proposes making this model the frontend
//! IR; its round-trip mode currently exercises that boundary.

mod adverb_origins;
mod anon_state;
mod assign_lower;
mod assign_target;
mod assign_writeback;
mod atomic_op;
mod attribute;
mod attribute_decl;
mod attribute_group;
mod bare_prefix;
mod bareword;
mod call_origins;
mod callable_context;
mod capture_term;
mod chain;
mod class_constructor;
mod compiler_var;
mod composition_source;
pub(crate) mod compound;
mod compound_stmt;
mod conditional_lower;
mod constructor_positional;
mod contextualizer;
mod control_signature;
mod decl_origins;
pub(crate) mod dotty_assign;
pub(crate) mod empty_fields;
mod expr_origins;
pub(crate) mod hyper_assign;
mod if_branch;
pub(crate) mod list_infix;
mod loop_bind;
pub(crate) mod nested_methods;
mod package_trait_origins;
mod parsed_view;
pub(crate) mod pointy_bind;
mod regex_origins;
pub(crate) mod sections;
pub(crate) mod signature_expand;
mod trait_is_constructor;
mod with_branch;
use constructor_positional::single_positional_class;
mod convert;
mod core_term_names;
use crate::core_type_names;
mod decl_compound_assign;
mod decl_modifier_split;
mod sigilless_lower;
mod supply_lower;
mod will_decl;
pub(crate) use decl_compound_assign::ZIP_ASSIGN;
mod decl_tail;
mod decl_traits;
mod declarator_docs;
mod declare_how;
mod declared_routines;
mod deferred_name;
mod derived_methods;
mod doc_phaser;
mod dynamic_method;
mod enum_term;
mod expr_block_origins;
mod feed_op;
mod fields;
mod formatter;
pub(crate) mod frontend;
mod hash_literal;
pub(crate) mod hierarchy;
mod hyper_prefix;
mod hyper_subscript;
mod immediate_call;
mod infix_func;
pub(crate) use infix_func::{add_colonpair, with_caller_operators};
mod assign_ro;
mod infix_origins;
mod infix_spelling;
mod inherited_fields;
mod keyed_hash;
mod label_context;
mod list_origins;
mod literal_parameter;
mod lower;
mod match_vars;
mod meta_infix;
mod method_assign_decl;
mod modifier_lower;
mod multidim_splat;
mod name_colonpairs;
mod name_parts;
mod named_ctor;
mod named_param;
mod negated_set;
mod nesting;
mod nf_quote;
mod operator_origins;
mod origin;
mod package_header;
mod package_traits;
mod parameter_destructure;
mod parameter_shape;
mod parameter_signature;
mod passthrough;
mod phaser_condition;
mod placeholder;
mod postfix_grouping;
mod prefix_call;
mod proto;
mod quoted_string;
mod react;
mod regex_char_class;
mod regex_code;
mod regex_decl_constructor;
mod regex_declaration;
mod regex_enumeration;
mod regex_extension;
mod regex_quantifier;
mod regex_sym;
mod regex_variable_alias;
mod render;
mod resolve;
mod role;
mod role_header;
mod routine_ctor;
mod routine_is_traits;
mod routine_lower;
mod routine_origins;
mod routine_source;
mod routine_traits;
mod shadowed_terms;
mod signature_decl;
mod signature_decl_origins;
mod signature_literal;
mod source_identifier;
#[cfg(test)]
mod source_identifier_tests;
mod string_origins;
mod subclass;
mod subscript_adverb;
mod substitution;
mod symbolic_deref;
mod temporize;
mod term_name;
mod thunk;
mod type_args;
mod type_build;
mod type_call;
mod type_capture_indirect;
mod type_ctor;
mod type_lower;
mod underscore_bindings;
pub(crate) mod unit_source;
mod unspell;
mod use_origins;
mod use_stmt;
mod var_decl_constructor;
mod var_does;
mod var_will;
mod word_list;
mod word_quote;

pub use formatter::formatter_ast;
pub use lower::lower;
pub(crate) use name_parts::is_name_part;
pub use subclass::gist as subclass_gist;
pub(crate) use subclass::is_node_value;

use crate::value::{RuntimeError, Value, ValueView};

/// A single RakuAST node: its class plus ordered fields. Immutable tree.
#[derive(Debug, Clone, PartialEq)]
pub struct RakuAstNode {
    pub class: RakuAstClass,
    pub fields: Vec<RakuAstField>,
    /// Where the parser read the node from (ADR-10723 §4): a member, not a
    /// field, so it never shows in the constructor form and no code that
    /// walks the fields sees it.
    pub origin: NodeOrigin,
}

/// A node's `RakuAST::Origin`, or none (a hand-built node, a node `convert`
/// made below the statement level), and the origins of the empty nodes its
/// absent fields stand for ([`empty_fields`]). It takes no part in a node's
/// equality: two trees that differ only in where they were written are the
/// same tree.
#[derive(Debug, Clone, Default)]
pub struct NodeOrigin(
    pub Option<Value>,
    pub empty_fields::EmptyOrigins,
    /// What `RakuAST::Lookup.resolution` answers (see [`resolve`]).
    pub Option<Value>,
);

impl PartialEq for NodeOrigin {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
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
    StatementEmpty,
    // Internal: a subtree the executing round trip passes through untouched
    // (`passthrough`); never rendered, never reachable from user code.
    ParsedLeaf,
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
    // `:(Int $x)`: a signature literal, `Signature` as its one positional field.
    FakeSignature,
    StrLiteral,
    QuotedString,
    // `q:to/END/`: a `QuotedString` that also keeps its `stop` line (ADR-12199).
    Heredoc,
    QuotedRegex,
    // Internal origin tag for a source `rx/.../` term. It is rendered and
    // accessed as the same public RakuAST class; the tag only preserves the
    // Regex value form across this boundary.
    QuotedRegexRx,
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
    VarCompilerBlock,
    VarCompilerRoutine,
    VarCompilerLookup,
    VarCompilerFile,
    VarCompilerLine,
    VarCompilerDistribution,
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
    PrefixMultislice,
    ApplyPostfix,
    Postfix,
    Assignment,
    MetaInfixAssign,
    MetaInfixNegate,
    BracketedInfix,
    RegexSequentialConjunction,
    RegexConjunction,
    RegexBacktrackModifiedAtom,
    RegexQuantifierBlockRange,
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
    CallBlockMethod,
    CallTermAsMethod,
    FlipFlop,
    Feed,
    StatementPrefixEager,
    TermCapture,
    // `<<"a b">>`: a quoted word inside a `quotewords` string.
    QuoteWordsAtom,
    MetaInfixReverse,
    MetaInfixCross,
    MetaInfixZip,
    CallMethod,
    // Phase 2 slice 23: quoted method names.
    CallQuotedMethod,
    // Phase 2 slice 26: hyper method calls.
    MetaPostfixHyper,
    // Hyper prefix operators (`-« @a`).
    MetaPrefixHyper,
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
    // `TERM .meth`: `ApplyDottyInfix(left, DottyInfix::Call, Call::Method)`.
    DottyInfixCall,
    // Phase 2 slice 10: scoped/typed variable declarations.
    TypeSimple,
    // `enum Color <Red Green>` -> `Type::Enum(name, term)`.
    TypeEnum,
    // Phase 2 slice 20: definite types (`Int:D` / `Int:U`).
    TypeDefinedness,
    TypeAnyDefinedness,
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
    /// NativeCall type declaration (`native N is Int ... { }`).
    Native,
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
    StatementModifierWhile,
    StatementModifierUntil,
    StatementModifierGiven,
    StatementModifierIf,
    StatementModifierUnless,
    StatementModifierWith,
    StatementModifierWithout,
    StatementModifierWhen,
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
    // The `∅` empty-set term.
    TermEmptySet,
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
    /// `will PHASE { ... }` on a variable declaration.
    TraitWill,
    TraitTrusts,
    TraitDoes,
    /// `class A hides B`.
    TraitHides,
    /// `trusts B` in a class body.
    StatementTrusts,
    StatementPrefixOnce,
    Nqp,
    /// `nqp::const::NAME`, a constant of the nqp backend.
    NqpConst,
    StatementPrefixStart,
    StatementPrefixQuietly,
    StatementPrefixSink,
    StatementPrefixLazy,
    StatementPrefixHyper,
    StatementPrefixRace,
    Mixin,
    VarAttribute,
    VarAttributePublic,
    CallPrivateMethod,
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
    TermDeclaration,
    /// `...` / `!!!` / `???` (the yada-yada stubs).
    StubFail,
    StubDie,
    StubWarn,
    // An argument-less core `use` pragma (`use strict`, `use fatal`, ...).
    Pragma,
    StatementUse,
    // `require Module;` as a model node and a lowered statement.
    StatementRequire,
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
            ParsedLeaf => "RakuAST::Internal::ParsedLeaf",
            StatementExpression => "RakuAST::Statement::Expression",
            StatementEmpty => "RakuAST::Statement::Empty",
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
            FakeSignature => "RakuAST::FakeSignature",
            StrLiteral => "RakuAST::StrLiteral",
            QuotedString => "RakuAST::QuotedString",
            Heredoc => "RakuAST::Heredoc",
            QuotedRegex => "RakuAST::QuotedRegex",
            QuotedRegexRx => "RakuAST::QuotedRegex",
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
            VarCompilerBlock => "RakuAST::Var::Compiler::Block",
            VarCompilerRoutine => "RakuAST::Var::Compiler::Routine",
            VarCompilerLookup => "RakuAST::Var::Compiler::Lookup",
            VarCompilerFile => "RakuAST::Var::Compiler::File",
            VarCompilerLine => "RakuAST::Var::Compiler::Line",
            VarCompilerDistribution => "RakuAST::Var::Compiler::Distribution",
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
            PrefixMultislice => "RakuAST::Prefix::Multislice",
            ApplyPostfix => "RakuAST::ApplyPostfix",
            Postfix => "RakuAST::Postfix",
            Assignment => "RakuAST::Assignment",
            MetaInfixAssign => "RakuAST::MetaInfix::Assign",
            MetaInfixNegate => "RakuAST::MetaInfix::Negate",
            BracketedInfix => "RakuAST::BracketedInfix",
            RegexSequentialConjunction => "RakuAST::Regex::SequentialConjunction",
            RegexConjunction => "RakuAST::Regex::Conjunction",
            RegexBacktrackModifiedAtom => "RakuAST::Regex::BacktrackModifiedAtom",
            RegexQuantifierBlockRange => "RakuAST::Regex::Quantifier::BlockRange",
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
            CallBlockMethod => "RakuAST::Call::BlockMethod",
            CallTermAsMethod => "RakuAST::Call::TermAsMethod",
            FlipFlop => "RakuAST::FlipFlop",
            Feed => "RakuAST::Feed",
            StatementPrefixEager => "RakuAST::StatementPrefix::Eager",
            TermCapture => "RakuAST::Term::Capture",
            QuoteWordsAtom => "RakuAST::QuoteWordsAtom",
            MetaInfixReverse => "RakuAST::MetaInfix::Reverse",
            MetaInfixCross => "RakuAST::MetaInfix::Cross",
            MetaInfixZip => "RakuAST::MetaInfix::Zip",
            CallMethod => "RakuAST::Call::Method",
            CallQuotedMethod => "RakuAST::Call::QuotedMethod",
            MetaPostfixHyper => "RakuAST::MetaPostfix::Hyper",
            MetaPrefixHyper => "RakuAST::MetaPrefix::Hyper",
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
            DottyInfixCall => "RakuAST::DottyInfix::Call",
            TypeSimple => "RakuAST::Type::Simple",
            TypeEnum => "RakuAST::Type::Enum",
            TypeDefinedness => "RakuAST::Type::Definedness",
            TypeAnyDefinedness => "RakuAST::Type::AnyDefinedness",
            TraitWillBuild => "RakuAST::Trait::WillBuild",
            TraitReturns => "RakuAST::Trait::Returns",
            TraitOf => "RakuAST::Trait::Of",
            TypeParameterized => "RakuAST::Type::Parameterized",
            TypeCoercion => "RakuAST::Type::Coercion",
            TypeCapture => "RakuAST::Type::Capture",
            Class => "RakuAST::Class",
            Native => "RakuAST::Native",
            Method => "RakuAST::Method",
            Role => "RakuAST::Role",
            RoleBody => "RakuAST::RoleBody",
            Label => "RakuAST::Label",
            StatementGiven => "RakuAST::Statement::Given",
            StatementWhen => "RakuAST::Statement::When",
            StatementDefault => "RakuAST::Statement::Default",
            StatementModifierFor => "RakuAST::StatementModifier::For",
            StatementModifierWhile => "RakuAST::StatementModifier::While",
            StatementModifierUntil => "RakuAST::StatementModifier::Until",
            StatementModifierGiven => "RakuAST::StatementModifier::Given",
            StatementModifierIf => "RakuAST::StatementModifier::If",
            StatementModifierUnless => "RakuAST::StatementModifier::Unless",
            StatementModifierWith => "RakuAST::StatementModifier::With",
            StatementModifierWhen => "RakuAST::StatementModifier::When",
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
            TermEmptySet => "RakuAST::Term::EmptySet",
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
            TraitWill => "RakuAST::Trait::Will",
            TraitTrusts => "RakuAST::Trait::Trusts",
            TraitDoes => "RakuAST::Trait::Does",
            TraitHides => "RakuAST::Trait::Hides",
            StatementTrusts => "RakuAST::Statement::Trusts",
            StatementPrefixOnce => "RakuAST::StatementPrefix::Once",
            Nqp => "RakuAST::Nqp",
            NqpConst => "RakuAST::Nqp::Const",
            StatementPrefixStart => "RakuAST::StatementPrefix::Start",
            StatementPrefixQuietly => "RakuAST::StatementPrefix::Quietly",
            StatementPrefixSink => "RakuAST::StatementPrefix::Sink",
            StatementPrefixLazy => "RakuAST::StatementPrefix::Lazy",
            StatementPrefixHyper => "RakuAST::StatementPrefix::Hyper",
            StatementPrefixRace => "RakuAST::StatementPrefix::Race",
            Mixin => "RakuAST::Mixin",
            VarAttribute => "RakuAST::Var::Attribute",
            VarAttributePublic => "RakuAST::Var::Attribute::Public",
            CallPrivateMethod => "RakuAST::Call::PrivateMethod",
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
            TermDeclaration => "RakuAST::Term::Declaration",
            StubFail => "RakuAST::Stub::Fail",
            StubDie => "RakuAST::Stub::Die",
            StubWarn => "RakuAST::Stub::Warn",
            Pragma => "RakuAST::Pragma",
            StatementUse => "RakuAST::Statement::Use",
            StatementRequire => "RakuAST::Statement::Require",
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
                | RakuAstClass::CallTerm
                | RakuAstClass::DottyInfixCallAssign
                | RakuAstClass::DottyInfixCall
                | RakuAstClass::TermWhatever
                | RakuAstClass::TermEmptySet
                | RakuAstClass::PrefixMultislice
                | RakuAstClass::WhateverCodeArgument
                | RakuAstClass::TermHyperWhatever
                | RakuAstClass::TermSelf
                | RakuAstClass::TermDeclaration
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
                | RakuAstClass::StatementEmpty
                | RakuAstClass::VarCompilerBlock
                | RakuAstClass::VarCompilerRoutine
                | RakuAstClass::VarCompilerDistribution
                | RakuAstClass::RegexQuantifierRange
                | RakuAstClass::RegexQuantifierBlockRange
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
}

/// Whether a RakuAST type object lists another in its MRO (`.^isa`; roles
/// excluded). The hierarchy is rakudo's measured one ([`hierarchy`]); a
/// mutsu-only node class answers through the rakudo class it stands in for.
pub fn type_object_isa(actual: &str, expected: &str) -> bool {
    hierarchy::isa(measured_name(actual), expected).unwrap_or(false)
        || (actual == expected && is_registered_type_object(actual))
}

/// Whether a RakuAST type object or node of class `actual` smartmatches the
/// RakuAST type `expected`: itself, an MRO entry or a role it does.
pub fn type_object_does(actual: &str, expected: &str) -> bool {
    hierarchy::does(measured_name(actual), expected).unwrap_or(false)
        || (actual == expected && is_registered_type_object(actual))
}

/// The `.^mro` of a RakuAST type object, as rakudo prints it.
pub fn type_object_mro(class_name: &str) -> Option<Vec<String>> {
    let measured = measured_name(class_name);
    let mut mro = hierarchy::mro(measured)?;
    if measured != class_name {
        mro.insert(0, class_name.to_string());
    }
    Some(mro)
}

/// How far `expected` is up the measured hierarchy of the RakuAST class
/// `actual`, for multi-dispatch ranking, without allocating: its `.^mro`
/// position for a class (`Any`/`Mu` after the RakuAST classes), and the
/// position after the last RakuAST class for a role it does. `None` when
/// `actual` is not a measured name; `Some(None)` when unrelated.
// Cost: O(d + r), d = MRO depth, r = roles of `actual`.
pub fn type_object_distance(actual: &str, expected: &str) -> Option<Option<usize>> {
    let measured = measured_name(actual);
    let entry = hierarchy::entry(measured)?;
    let offset = usize::from(measured != actual);
    if expected == actual || expected == measured {
        return Some(Some(if expected == actual { 0 } else { offset }));
    }
    let classes = if entry.kind == hierarchy::Kind::Class {
        entry.mro.len()
    } else {
        0
    };
    if let Some(i) = entry.mro.iter().position(|a| *a == expected) {
        return Some(Some(i + 1 + offset));
    }
    if entry.kind == hierarchy::Kind::Class {
        if expected == "Any" {
            return Some(Some(classes + 1 + offset));
        }
        if expected == "Mu" {
            return Some(Some(classes + 2 + offset));
        }
    }
    if entry.roles.contains(&expected) {
        return Some(Some(classes + 1 + offset));
    }
    Some(None)
}

/// Every RakuAST name a node or type object of class `class_name` conforms to
/// (the class it is measured as, its MRO without `Any`/`Mu`, and its roles),
/// excluding `class_name` itself; `None` when it is not a measured name. The
/// set [`type_object_distance`] ranks by, for a memo of the relation.
// Cost: O(d + r), d = MRO depth, r = roles of `class_name`.
pub(crate) fn type_object_conforms_to(class_name: &str) -> Option<Vec<String>> {
    let measured = measured_name(class_name);
    let entry = hierarchy::entry(measured)?;
    let mut names: Vec<String> = Vec::with_capacity(entry.mro.len() + entry.roles.len() + 1);
    if measured != class_name {
        names.push(measured.to_string());
    }
    names.extend(
        entry
            .mro
            .iter()
            .chain(entry.roles.iter())
            .map(|n| n.to_string()),
    );
    Some(names)
}

/// The `.^roles` of a RakuAST type object, as rakudo prints them.
pub fn type_object_roles(class_name: &str) -> Option<Vec<&'static str>> {
    hierarchy::entry(measured_name(class_name)).map(|e| e.roles.clone())
}

/// Whether a RakuAST name is a role (`RakuAST::Code`, `RakuAST::Sinkable`).
pub fn type_object_is_role(class_name: &str) -> bool {
    hierarchy::entry(class_name).is_some_and(|e| e.kind == hierarchy::Kind::Role)
}

/// Whether a RakuAST name is a bare namespace (`RakuAST::IMPL`), not a type.
pub fn type_object_is_package(class_name: &str) -> bool {
    hierarchy::entry(class_name).is_some_and(|e| e.kind == hierarchy::Kind::Package)
}

/// The rakudo class a mutsu-only node class answers the hierarchy as. mutsu
/// models two spellings rakudo folds into another class: `Call::BlockMethod`
/// (rakudo renders `$x.&{...}` as a `Call::TermAsMethod`) and
/// `Name::Part::EmptyEdge` (an `Empty` part at a name's edge).
fn measured_name(class_name: &str) -> &str {
    match class_name {
        "RakuAST::Call::BlockMethod" => "RakuAST::Call::TermAsMethod",
        "RakuAST::Name::Part::EmptyEdge" => "RakuAST::Name::Part::Empty",
        _ => class_name,
    }
}

/// The immediate parent in mutsu's linearized RakuAST model hierarchy.
pub fn type_object_direct_parent(class_name: &str) -> Option<String> {
    type_object_mro(class_name)?.into_iter().nth(1)
}

/// Whether `class_name` names a RakuAST type object mutsu models (a node
/// class or one of its abstract ancestors).
pub(crate) fn is_registered_type_object(class_name: &str) -> bool {
    if hierarchy::entry(class_name).is_some() {
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
    RakuAstClass::StatementEmpty,
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
    RakuAstClass::FakeSignature,
    RakuAstClass::StrLiteral,
    RakuAstClass::QuotedString,
    RakuAstClass::Heredoc,
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
    RakuAstClass::RegexCharClass(regex_char_class::RegexCharClassKind::BackSpace),
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
    RakuAstClass::VarCompilerBlock,
    RakuAstClass::VarCompilerRoutine,
    RakuAstClass::VarCompilerLookup,
    RakuAstClass::VarCompilerFile,
    RakuAstClass::VarCompilerLine,
    RakuAstClass::VarCompilerDistribution,
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
    RakuAstClass::MetaInfixNegate,
    RakuAstClass::BracketedInfix,
    RakuAstClass::RegexSequentialConjunction,
    RakuAstClass::RegexConjunction,
    RakuAstClass::RegexBacktrackModifiedAtom,
    RakuAstClass::RegexQuantifierBlockRange,
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
    RakuAstClass::CallBlockMethod,
    RakuAstClass::CallTermAsMethod,
    RakuAstClass::FlipFlop,
    RakuAstClass::Feed,
    RakuAstClass::StatementPrefixEager,
    RakuAstClass::TermCapture,
    RakuAstClass::QuoteWordsAtom,
    RakuAstClass::MetaInfixReverse,
    RakuAstClass::MetaInfixCross,
    RakuAstClass::MetaInfixZip,
    RakuAstClass::CallMethod,
    RakuAstClass::CallQuotedMethod,
    RakuAstClass::MetaPostfixHyper,
    RakuAstClass::MetaPrefixHyper,
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
    RakuAstClass::DottyInfixCall,
    RakuAstClass::TypeSimple,
    RakuAstClass::TypeEnum,
    RakuAstClass::TypeDefinedness,
    RakuAstClass::TypeAnyDefinedness,
    RakuAstClass::TraitWillBuild,
    RakuAstClass::TraitReturns,
    RakuAstClass::TraitOf,
    RakuAstClass::TypeParameterized,
    RakuAstClass::TypeCoercion,
    RakuAstClass::TypeCapture,
    RakuAstClass::Class,
    RakuAstClass::Native,
    RakuAstClass::Method,
    RakuAstClass::Role,
    RakuAstClass::RoleBody,
    RakuAstClass::Label,
    RakuAstClass::StatementGiven,
    RakuAstClass::StatementWhen,
    RakuAstClass::StatementDefault,
    RakuAstClass::StatementModifierFor,
    RakuAstClass::StatementModifierWhile,
    RakuAstClass::StatementModifierUntil,
    RakuAstClass::StatementModifierGiven,
    RakuAstClass::StatementModifierIf,
    RakuAstClass::StatementModifierUnless,
    RakuAstClass::StatementModifierWith,
    RakuAstClass::StatementModifierWhen,
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
    RakuAstClass::TermEmptySet,
    RakuAstClass::PrefixMultislice,
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
    RakuAstClass::TraitWill,
    RakuAstClass::TraitTrusts,
    RakuAstClass::TraitDoes,
    RakuAstClass::TraitHides,
    RakuAstClass::StatementTrusts,
    RakuAstClass::StatementPrefixOnce,
    RakuAstClass::Nqp,
    RakuAstClass::NqpConst,
    RakuAstClass::StatementPrefixStart,
    RakuAstClass::StatementPrefixQuietly,
    RakuAstClass::StatementPrefixSink,
    RakuAstClass::StatementPrefixLazy,
    RakuAstClass::StatementPrefixHyper,
    RakuAstClass::StatementPrefixRace,
    RakuAstClass::Mixin,
    RakuAstClass::VarAttribute,
    RakuAstClass::VarAttributePublic,
    RakuAstClass::CallPrivateMethod,
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
    RakuAstClass::TermDeclaration,
    RakuAstClass::StubFail,
    RakuAstClass::StubDie,
    RakuAstClass::StubWarn,
    RakuAstClass::Pragma,
    RakuAstClass::StatementUse,
    RakuAstClass::StatementRequire,
    RakuAstClass::StatementNeed,
    RakuAstClass::StatementImport,
    RakuAstClass::StatementLanguageVersion,
];

/// Whether an assignment in expression position assigns to `target`
/// (`assign_lower`, ADR-10723 Stage 2).
// Cost: O(d), d = levels of parentheses around the target.
pub(crate) fn is_expression_assign_target(target: &crate::ast::Expr) -> bool {
    assign_lower::is_expression_target(target)
}

/// Whether the statement parser assigns to `target` by writing through its
/// result (`assign_lower`).
// Cost: O(d), d = levels of parentheses around the target.
pub(crate) fn is_callable_lvalue_assign_target(target: &crate::ast::Expr) -> bool {
    assign_lower::is_callable_lvalue_target(target)
}

/// Whether `target` is an item contextualizer the statement parser assigns
/// through (`$(EXPR) = v`).
// Cost: O(d), d = levels of parentheses around the target.
pub(crate) fn is_item_assign_target(target: &crate::ast::Expr) -> bool {
    assign_lower::is_item_target(target)
}

/// Lower an assignment in expression position: the one lowering of the
/// assignment family, shared with `EVAL` of a hand-built node.
// Cost: O(n), n = size of the target and source.
pub(crate) fn lower_expression_assign(node: crate::ast::parsed::Assign) -> crate::ast::Expr {
    assign_lower::lower_expression(node)
}

/// Lower an assignment written as a statement: the one lowering of the
/// assignment family, shared with `EVAL` of a hand-built node.
// Cost: O(n), n = size of the target and source.
pub(crate) fn lower_statement_assign(node: crate::ast::parsed::Assign) -> crate::ast::Stmt {
    assign_lower::lower_statement(node)
}

/// Lower a construct the parser built in its RakuAST shape (ADR-10723
/// Stage 2): the one lowering of its family, shared with `EVAL` of a
/// hand-built node.
// Cost: O(n), n = size of the construct.
pub(crate) fn lower_parsed_stmt(
    node: crate::ast::parsed::ParsedStmt,
) -> Result<crate::ast::Stmt, RuntimeError> {
    conditional_lower::lower_parsed(node)
}

/// The name a `RakuAST::Sub` node declares (`foo` of `sub foo { }`), if it has one.
// Cost: O(f), f = fields of the node.
pub(crate) fn declared_sub_name(node: &RakuAstNode) -> Option<String> {
    (node.class == RakuAstClass::Sub)
        .then(|| declared_routines::declared_by(node))
        .flatten()
}

/// The `(package, name)` of a `Method` of the unit tree `$*CU` answers.
// Cost: O(f), f = fields of the node.
pub(crate) fn declared_method(node: &RakuAstNode) -> Option<(String, String)> {
    resolve::declared_method(node)
}

/// The `VarDeclaration::Simple` annotated onto a `ParameterTarget::Var` of the
/// unit tree `$*CU` answers (see [`resolve`]).
// Cost: O(1).
pub(crate) fn annotated_target_declaration(node: &RakuAstNode) -> Option<Value> {
    resolve::target_declaration(node)
}

/// The parameter a `RakuAST::Parameter` node declares, as the `ParamDef` the
/// parser builds for it (`Parameter.meta-object` is made from it).
// Cost: O(size of the parameter node).
pub(crate) fn parameter_param_def(
    node: &RakuAstNode,
) -> Result<crate::ast::ParamDef, RuntimeError> {
    lower::lower_parameter_def(node)
}

/// Entry point for `Str.AST`: parse the source, convert, wrap in `Value::RakuAst`.
pub fn str_dot_ast(source: &str) -> Result<Value, RuntimeError> {
    let _origins_wanted = crate::parser::term_origins::Wanted::enter();
    let (mut stmts, _finish) = crate::parse_dispatch::parse_source_spelled(source)?;
    // The origins of the tree index this text (ADR-10723 §4).
    let _source = unit_source::Scope::enter(source);
    let _origins = expr_origins::strip(&mut stmts);
    let mut node = convert::statement_list(&stmts)?;
    origin::set_span(&mut node, origin::unit_list_span(&stmts, source));
    Ok(Value::rakuast(Box::new(node)))
}

/// The `RakuAST::CompUnit` of the unit being compiled, as `$*CU` answers it:
/// [`str_dot_ast_compunit`] whose lookups carry their resolution (#15610).
// Cost: O(n * d), n = nodes of the tree, d = scope depth.
pub fn compile_time_compunit(
    source: &str,
    imported: &dyn Fn(&str) -> bool,
) -> Result<Value, RuntimeError> {
    let cu = str_dot_ast_compunit(source)?;
    let ValueView::RakuAst(node) = cu.view() else {
        return Ok(cu);
    };
    let mut node = RakuAstNode::clone(node);
    resolve::annotate(&mut node, imported);
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
        origin: Default::default(),
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
// Cost: O(n), n = arguments and contained list elements or identifier bytes.
pub fn construct(
    class_name: &str,
    method: &str,
    args: &[Value],
) -> Result<Option<Value>, RuntimeError> {
    // An array element or a bound variable arrives as its `ContainerRef` cell
    // (`Name.from-identifier(@parts.head)`); a node stores the value inside.
    if args
        .iter()
        .any(|arg| matches!(arg.view(), ValueView::ContainerRef(_)))
    {
        let read: Vec<Value> = args.iter().map(Value::deref_container).collect();
        return construct_values(class_name, method, &read);
    }
    construct_values(class_name, method, args)
}

fn construct_values(
    class_name: &str,
    method: &str,
    args: &[Value],
) -> Result<Option<Value>, RuntimeError> {
    if method == "new"
        && let Some(result) = routine_ctor::construct(class_name, args)
    {
        return result.map(Some);
    }
    if method == "new"
        && let Some(result) = named_ctor::construct(class_name, args)
    {
        return result.map(Some);
    }
    if method == "new"
        && let Some(result) = package_traits::construct(class_name, args)
    {
        return result.map(Some);
    }
    if method == "new"
        && let Some(result) = control_signature::construct(class_name, args)
    {
        return result.map(Some);
    }
    if method == "new"
        && let Some(node) = subscript_adverb::construct(class_name, args)
    {
        return node.map(Some);
    }
    if method == "new"
        && let Some(node) = type_ctor::construct(class_name, args)
    {
        return node.map(Some);
    }
    if matches!(class_name, "RakuAST::QuotedString" | "RakuAST::Heredoc") && method == "new" {
        let heredoc = class_name == "RakuAST::Heredoc";
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
        if heredoc {
            let stop = named_arg(args, "stop")
                .filter(|stop| matches!(stop.view(), ValueView::Str(_)))
                .ok_or_else(|| {
                    RuntimeError::new("RakuAST::Heredoc.new requires a string `stop`")
                })?;
            fields.push(RakuAstField {
                name: Some("stop"),
                value: RakuAstFieldValue::Node(stop),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: if heredoc {
                RakuAstClass::Heredoc
            } else {
                RakuAstClass::QuotedString
            },
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
        let mut fields = Vec::with_capacity(5);
        if let Some(scope) =
            named_arg(args, "scope").filter(|s| s.as_str().is_some_and(|s| !s.is_empty()))
        {
            fields.push(RakuAstField {
                name: Some("scope"),
                value: RakuAstFieldValue::Node(scope),
            });
        }
        fields.push(RakuAstField {
            name: Some("name"),
            value: RakuAstFieldValue::Node(name),
        });
        if let Some(of) = named_arg(args, "of").filter(|v| !v.is_nil()) {
            fields.push(RakuAstField {
                name: Some("of"),
                value: RakuAstFieldValue::Node(of),
            });
        }
        if let Some(traits) = named_arg(args, "traits")
            && traits
                .as_list_items()
                .is_some_and(|items| !items.is_empty())
        {
            fields.push(RakuAstField {
                name: Some("traits"),
                value: RakuAstFieldValue::List(rakuast_node_list(
                    &traits,
                    "RakuAST::Type::Enum.new",
                    "traits",
                )?),
            });
        }
        fields.push(RakuAstField {
            name: Some("term"),
            value: RakuAstFieldValue::Node(term),
        });
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TypeEnum,
            fields,
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
        // `$x.?m` / `$x.+m` / `$x.*m`: a method call's dispatcher.
        if class == RakuAstClass::CallMethod
            && let Some(dispatch) = named_arg(args, "dispatch")
            && dispatch.as_str().is_some_and(|d| !d.is_empty())
        {
            fields.push(RakuAstField {
                name: Some("dispatch"),
                value: RakuAstFieldValue::Node(dispatch),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
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
            origin: Default::default(),
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
                    origin: Default::default(),
                    class: RakuAstClass::NamePartSimple,
                    fields: vec![RakuAstField {
                        name: None,
                        value: RakuAstFieldValue::Node(Value::str(name.to_string())),
                    }],
                })))
            })
            .collect::<Result<Vec<_>, RuntimeError>>()?;
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
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
            origin: Default::default(),
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
            origin: Default::default(),
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
            origin: Default::default(),
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
            origin: Default::default(),
            class: RakuAstClass::Blockoid,
            fields: vec![RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(args[0].clone()),
            }],
        }))));
    }
    if class_name == "RakuAST::Signature" && method == "new" {
        // `parameters` is kept only when given: `Signature.new` and
        // `Signature.new(parameters => ())` differ on rakudo
        // (`parameters-initialized`).
        let given = named_arg(args, "parameters").is_some();
        let parameters = named_arg(args, "parameters")
            .map(|value| {
                value
                    .as_list_items()
                    .map(|items| {
                        items
                            .iter()
                            .map(|item| item.with_deref(|value| value.descalarize().clone()))
                            .collect::<Vec<_>>()
                    })
                    .ok_or_else(|| {
                        RuntimeError::new(
                            "RakuAST::Signature.new expects `parameters` to be a list",
                        )
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
        let mut fields = Vec::new();
        if given {
            fields.push(RakuAstField {
                name: Some("parameters"),
                value: RakuAstFieldValue::List(parameters),
            });
        }
        if let Some(returns) = returns {
            fields.push(RakuAstField {
                name: Some("returns"),
                value: RakuAstFieldValue::Node(returns),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::Signature,
            fields,
        }))));
    }
    if class_name == "RakuAST::VarDeclaration::Signature" && method == "new" {
        return signature_decl::construct(args).map(Some);
    }
    if class_name == "RakuAST::Parameter" && method == "new" {
        let target = named_arg(args, "target");
        // A sigilless parameter (`|c`, `\x`) targets a `ParameterTarget::Term`.
        if let Some(target) = &target
            && !matches!(target.view(), ValueView::RakuAst(node)
                if node.class == RakuAstClass::ParameterTargetTerm)
        {
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
        if let Some(default_rw) = named_arg(args, "default-rw") {
            if !matches!(default_rw.view(), ValueView::Bool(_)) {
                return Err(RuntimeError::new(
                    "RakuAST::Parameter.new expects `default-rw` to be Bool",
                ));
            }
            fields.push(RakuAstField {
                name: Some("default-rw"),
                value: RakuAstFieldValue::Node(default_rw),
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
        if let Some(array_shape) = named_arg(args, "array-shape") {
            require_any_rakuast(&array_shape, "RakuAST::Parameter.new", "array-shape")?;
            fields.push(RakuAstField {
                name: Some("array-shape"),
                value: RakuAstFieldValue::Node(array_shape),
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
        if let Some(value) = named_arg(args, "value") {
            fields.push(RakuAstField {
                name: Some("value"),
                value: RakuAstFieldValue::Node(value),
            });
        }
        if let Some(traits) = named_arg(args, "traits")
            && let Some(items) = traits.as_list_items()
            && !items.is_empty()
        {
            fields.push(RakuAstField {
                name: Some("traits"),
                value: RakuAstFieldValue::List(rakuast_node_list(
                    &traits,
                    "RakuAST::Parameter.new",
                    "traits",
                )?),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::Parameter,
            fields,
        }))));
    }
    if class_name == "RakuAST::VarDeclaration::Simple" && method == "new" {
        return var_decl_constructor::construct(args).map(Some);
    }
    if class_name == "RakuAST::VarDeclaration::Anonymous" && method == "new" {
        return var_decl_constructor::construct_anonymous(args).map(Some);
    }
    if class_name == "RakuAST::Class" && method == "new" {
        return class_constructor::construct(args).map(Some);
    }
    if class_name == "RakuAST::Native" && method == "new" {
        return class_constructor::construct_native(args).map(Some);
    }
    if class_name == "RakuAST::Trait::Is" && method == "new" {
        return trait_is_constructor::construct(args).map(Some);
    }
    if matches!(
        class_name,
        "RakuAST::Regex::Sequence"
            | "RakuAST::Regex::Alternation"
            | "RakuAST::Regex::SequentialAlternation"
            | "RakuAST::Regex::Conjunction"
            | "RakuAST::Regex::SequentialConjunction"
    ) && method == "new"
    {
        let class = if class_name.ends_with("Sequence") {
            RakuAstClass::RegexSequence
        } else if class_name.ends_with("SequentialAlternation") {
            RakuAstClass::RegexSequentialAlternation
        } else if class_name.ends_with("SequentialConjunction") {
            RakuAstClass::RegexSequentialConjunction
        } else if class_name.ends_with("Conjunction") {
            RakuAstClass::RegexConjunction
        } else {
            RakuAstClass::RegexAlternation
        };
        for argument in args {
            require_regex_node(argument, class_name)?;
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
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
        require_any_rakuast(&var, "RakuAST::Regex::Interpolation.new", "var")?;
        let sequential = named_arg(args, "sequential").unwrap_or_else(|| Value::truth(false));
        if !matches!(sequential.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::Interpolation.new expects `sequential` to be Bool",
            ));
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
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
        let hash = named_arg(args, "hash").unwrap_or_else(|| Value::truth(false));
        if !matches!(hash.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(
                "RakuAST::Regex::NamedCapture.new expects `hash` to be Bool",
            ));
        }
        if matches!(array.view(), ValueView::Bool(true))
            && matches!(hash.view(), ValueView::Bool(true))
        {
            return Err(RuntimeError::new(
                "RakuAST::Regex::NamedCapture.new cannot combine `array` and `hash`",
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
        if matches!(hash.view(), ValueView::Bool(true)) {
            fields.push(RakuAstField {
                name: Some("hash"),
                value: RakuAstFieldValue::Node(hash),
            });
        }
        fields.push(RakuAstField {
            name: Some("regex"),
            value: RakuAstFieldValue::Node(regex),
        });
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
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
            origin: Default::default(),
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
            origin: Default::default(),
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
            origin: Default::default(),
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
            origin: Default::default(),
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
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
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
            origin: Default::default(),
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
                if node.class.printed_name().starts_with("RakuAST::Regex::")
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
            origin: Default::default(),
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
            origin: Default::default(),
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
            origin: Default::default(),
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
            origin: Default::default(),
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
            origin: Default::default(),
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
            origin: Default::default(),
            class: RakuAstClass::QuotedRegex,
            fields,
        }))));
    }
    if method == "new" {
        if class_name == "RakuAST::Grammar" {
            return class_constructor::construct_grammar(args).map(Some);
        }
        if class_name == "RakuAST::Role" {
            return class_constructor::construct_role(args).map(Some);
        }
        if class_name == "RakuAST::RoleBody" {
            return class_constructor::construct_role_body(args).map(Some);
        }
        if let Some(result) = regex_decl_constructor::construct(class_name, args) {
            return result.map(Some);
        }
    }
    if let Some(node) = regex_char_class::construct(class_name, method, args) {
        return node.map(Some);
    }
    if let Some(node) = regex_enumeration::construct(class_name, method, args) {
        return node.map(Some);
    }
    if let Some(node) = role_header::construct(class_name, method, args) {
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
            origin: Default::default(),
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
            origin: Default::default(),
            class,
            fields: Vec::new(),
        }))));
    }
    // `Name.from-identifier("today", colonpairs => (...))`: the identifier and
    // its colonpairs, as a declared extended name converts (`name_colonpairs`).
    if class_name == "RakuAST::Name"
        && method == "from-identifier"
        && let Some(pairs) = named_arg(args, "colonpairs")
    {
        let identifier = args
            .iter()
            .find(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
            .filter(|a| a.as_str().is_some())
            .ok_or_else(|| {
                RuntimeError::new("RakuAST::Name.from-identifier expects an identifier")
            })?;
        let items = pairs.as_list_items().ok_or_else(|| {
            RuntimeError::new("RakuAST::Name.from-identifier expects `colonpairs` to be a list")
        })?;
        let mut fields = vec![RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(identifier.clone()),
        }];
        if !items.is_empty() {
            fields.push(RakuAstField {
                name: Some("colonpairs"),
                value: RakuAstFieldValue::List(
                    items
                        .iter()
                        .map(|item| item.with_deref(|v| v.descalarize().clone()))
                        .collect(),
                ),
            });
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::Name,
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
            origin: Default::default(),
            class,
            fields: vec![RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(args[0].clone()),
            }],
        }))));
    }
    // `HashComposer.new($expression?, :object-hash)` has one optional
    // positional expression and an optional boolean spelling flag.
    if class_name == "RakuAST::Circumfix::HashComposer" && method == "new" {
        let positional: Vec<_> = args
            .iter()
            .filter(|arg| !matches!(arg.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
            .collect();
        if positional.len() > 1 {
            return Err(RuntimeError::new(
                "RakuAST::Circumfix::HashComposer.new accepts at most one expression",
            ));
        }
        let mut fields = Vec::with_capacity(2);
        if let Some(expression) = positional.first() {
            require_any_rakuast(
                expression,
                "RakuAST::Circumfix::HashComposer.new",
                "expression",
            )?;
            fields.push(RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node((*expression).clone()),
            });
        }
        if let Some(object_hash) = named_arg(args, "object-hash") {
            let ValueView::Bool(enabled) = object_hash.view() else {
                return Err(RuntimeError::new(
                    "RakuAST::Circumfix::HashComposer.new expects `object-hash` to be Bool",
                ));
            };
            if enabled {
                fields.push(RakuAstField {
                    name: Some("object-hash"),
                    value: RakuAstFieldValue::Node(Value::truth(true)),
                });
            }
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::CircumfixHashComposer,
            fields,
        }))));
    }
    // `Statement::Use` / `Statement::Require`: a required `module-name` plus
    // optional named fields, kept in the order rakudo declares them.
    if method == "new"
        && let Some((class, optional)) = module_statement_schema(class_name)
    {
        let module_name = named_arg(args, "module-name").ok_or_else(|| {
            RuntimeError::new(format!(
                "{class_name}.new requires a `module-name` argument"
            ))
        })?;
        require_rakuast_class(
            &module_name,
            RakuAstClass::Name,
            &format!("{class_name}.new"),
        )?;
        let mut fields = vec![RakuAstField {
            name: Some("module-name"),
            value: RakuAstFieldValue::Node(module_name),
        }];
        for &fname in optional {
            if let Some(value) = named_arg(args, fname) {
                fields.push(RakuAstField {
                    name: Some(fname),
                    value: RakuAstFieldValue::Node(value),
                });
            }
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class,
            fields,
        }))));
    }
    // Multi-field named constructors: the named args map to same-named fields in
    // the class's schema order (`ApplyInfix.new(left => …, infix => …, right => …)`).
    if let Some((class, schema)) = multi_field_schema(class_name, method) {
        let mut fields = Vec::with_capacity(schema.len());
        if class == RakuAstClass::Block {
            for name in [
                "topic",
                "implicit-topic",
                "required-topic",
                "exception",
                "may-have-signature",
            ] {
                if named_arg(args, name).is_some_and(|value| value.truthy()) {
                    fields.push(convert::true_field(name));
                }
            }
        }
        if class == RakuAstClass::StatementExpression
            && let Some(labels) = named_arg(args, "labels")
        {
            let items = labels.as_list_items().ok_or_else(|| {
                RuntimeError::new("RakuAST::Statement::Expression.new expects a list of labels")
            })?;
            let mut labels = Vec::new();
            labels.try_reserve(items.len()).map_err(|_| {
                RuntimeError::new("RakuAST::Statement::Expression.new label list is too large")
            })?;
            for item in items {
                let item = item.with_deref(|value| value.descalarize().clone());
                require_rakuast_class(&item, RakuAstClass::Label, class_name)?;
                labels.push(item);
            }
            fields.push(RakuAstField {
                name: Some("labels"),
                value: RakuAstFieldValue::List(labels),
            });
        }
        for &fname in schema {
            let value = match named_arg(args, fname) {
                Some(value) => value,
                // An omitted pointy signature is a defined empty Signature
                // through its accessor, but is absent from constructor text.
                None if fname == "signature" && class == RakuAstClass::PointyBlock => continue,
                // A block's `body` defaults to an empty blockoid in raku:
                // `RakuAST::Block.new` is a complete (empty) block.
                None if fname == "body"
                    && matches!(class, RakuAstClass::Block | RakuAstClass::PointyBlock) =>
                {
                    empty_blockoid()
                }
                None => {
                    return Err(RuntimeError::new(format!(
                        "{class_name}.{method} requires a `{fname}` argument"
                    )));
                }
            };
            let value = if class == RakuAstClass::ApplyListInfix && fname == "operands" {
                let items = value.as_list_items().ok_or_else(|| {
                    RuntimeError::new("RakuAST::ApplyListInfix.new expects a list of operands")
                })?;
                let mut operands = Vec::new();
                operands.try_reserve(items.len()).map_err(|_| {
                    RuntimeError::new("RakuAST::ApplyListInfix.new operand list is too large")
                })?;
                for item in items {
                    let item = item.with_deref(|value| value.descalarize().clone());
                    require_any_rakuast(&item, class_name, fname)?;
                    operands.push(item);
                }
                RakuAstFieldValue::List(operands)
            } else {
                RakuAstFieldValue::Node(value)
            };
            fields.push(RakuAstField {
                name: Some(fname),
                value,
            });
        }
        let optional: &[&'static str] = match class {
            RakuAstClass::StatementExpression => &["condition-modifier", "loop-modifier"],
            RakuAstClass::CallBlockMethod => &["args"],
            _ => &[],
        };
        for &name in optional {
            if let Some(value) = named_arg(args, name) {
                require_any_rakuast(&value, class_name, name)?;
                fields.push(RakuAstField {
                    name: Some(name),
                    value: RakuAstFieldValue::Node(value),
                });
            }
        }
        return Ok(Some(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class,
            fields,
        }))));
    }
    Ok(None)
}

fn empty_blockoid() -> Value {
    let statements = Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementList,
        fields: Vec::new(),
    }));
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Blockoid,
        fields: vec![RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(statements),
        }],
    }))
}

fn empty_block() -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
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

/// A constructor's list-of-nodes argument (`traits => ($t,)`): each item's
/// content, which must be a RakuAST node. An item held in a variable arrives
/// as its container.
// Cost: O(n), n = list items.
fn rakuast_node_list(
    value: &Value,
    constructor: &str,
    argument: &str,
) -> Result<Vec<Value>, RuntimeError> {
    let items = value.as_list_items().ok_or_else(|| {
        RuntimeError::new(format!("{constructor} expects `{argument}` to be a list"))
    })?;
    let mut nodes = Vec::new();
    nodes
        .try_reserve(items.len())
        .map_err(|_| RuntimeError::new(format!("{constructor} `{argument}` list is too large")))?;
    for item in items {
        let node = item.with_deref(|v| v.descalarize().clone());
        require_any_rakuast(&node, constructor, argument)?;
        nodes.push(node);
    }
    Ok(nodes)
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
                    | RakuAstClass::RegexConjunction
                    | RakuAstClass::RegexSequentialConjunction
                    | RakuAstClass::RegexQuantifiedAtom
                    | RakuAstClass::RegexBacktrackModifiedAtom
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
                    | RakuAstClass::TypeAnyDefinedness
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

fn zero_positional_class(class_name: &str, method: &str) -> Option<RakuAstClass> {
    Some(match (class_name, method) {
        ("RakuAST::Var::Compiler::Block", "new") => RakuAstClass::VarCompilerBlock,
        ("RakuAST::Var::Compiler::Routine", "new") => RakuAstClass::VarCompilerRoutine,
        ("RakuAST::Var::Compiler::Distribution", "new") => RakuAstClass::VarCompilerDistribution,
        ("RakuAST::Statement::Empty", "new") => RakuAstClass::StatementEmpty,
        ("RakuAST::DottyInfix::Call", "new") => RakuAstClass::DottyInfixCall,
        ("RakuAST::DottyInfix::CallAssign", "new") => RakuAstClass::DottyInfixCallAssign,
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
        ("RakuAST::Term::EmptySet", "new") => RakuAstClass::TermEmptySet,
        ("RakuAST::Prefix::Multislice", "new") => RakuAstClass::PrefixMultislice,
        ("RakuAST::Term::HyperWhatever", "new") => RakuAstClass::TermHyperWhatever,
        ("RakuAST::Name::Part::Empty", "new") => RakuAstClass::NamePartEmpty,
        ("RakuAST::Name::Part::EmptyEdge", "new") => RakuAstClass::NamePartEmptyEdge,
        _ => return None,
    })
}

/// The class and optional named fields of a module-naming statement
/// (`module-name` is always required).
fn module_statement_schema(class_name: &str) -> Option<(RakuAstClass, &'static [&'static str])> {
    match class_name {
        "RakuAST::Statement::Use" => Some((RakuAstClass::StatementUse, &["argument"][..])),
        "RakuAST::Statement::Require" => {
            Some((RakuAstClass::StatementRequire, &["file", "argument"][..]))
        }
        _ => None,
    }
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
        ("RakuAST::Label", "new") => (RakuAstClass::Label, &["name"][..]),
        ("RakuAST::Call::BlockMethod", "new") => (RakuAstClass::CallBlockMethod, &["block"][..]),
        ("RakuAST::Statement::Whenever", "new") => {
            (RakuAstClass::StatementWhenever, &["trigger", "body"][..])
        }
        ("RakuAST::ApplyInfix", "new") => {
            (RakuAstClass::ApplyInfix, &["left", "infix", "right"][..])
        }
        ("RakuAST::ApplyListInfix", "new") => {
            (RakuAstClass::ApplyListInfix, &["infix", "operands"][..])
        }
        ("RakuAST::ApplyPrefix", "new") => (RakuAstClass::ApplyPrefix, &["prefix", "operand"][..]),
        ("RakuAST::ApplyDottyInfix", "new") => (
            RakuAstClass::ApplyDottyInfix,
            &["left", "infix", "right"][..],
        ),
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
        ("RakuAST::Trait::Will", "new") => (RakuAstClass::TraitWill, &["phase", "block"][..]),
        ("RakuAST::Trait::Trusts", "new") => (RakuAstClass::TraitTrusts, &["type"][..]),
        ("RakuAST::Statement::Trusts", "new") => (RakuAstClass::StatementTrusts, &["type"][..]),
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
// Cost: O(f), f = fields of the node.
/// Whether `field` is private state of `node` that rakudo keeps in an
/// attribute with no accessor and leaves out of `visit-children` (an
/// `ApplyListInfix`'s adverbs), or that rakudo does not keep at all but mutsu
/// needs for the round trip (the atom of a regex alias to a variable).
// Cost: O(1).
pub(crate) fn is_unvisited_field(node: &RakuAstNode, field: &RakuAstField) -> bool {
    infix_func::is_hidden_adverbs(node, field) || regex_variable_alias::is_source_field(node, field)
}

/// The line `node`'s origin begins on, if it has one.
// Cost: O(f), f = fields of the node.
pub(crate) fn origin_line_of(node: &RakuAstNode) -> Option<i64> {
    origin::line_of(node)
}

/// The lookup classes [`resolve`] gives a resolution (`is-resolved` and
/// `resolution` are theirs).
// Cost: O(1).
fn is_resolvable_lookup(class: RakuAstClass) -> bool {
    matches!(
        class,
        RakuAstClass::VarLexical
            | RakuAstClass::VarDynamic
            | RakuAstClass::Infix
            | RakuAstClass::Prefix
            | RakuAstClass::Postfix
            | RakuAstClass::PostcircumfixArrayIndex
            | RakuAstClass::PostcircumfixHashIndex
            | RakuAstClass::PostcircumfixLiteralHashIndex
            | RakuAstClass::CallName
            | RakuAstClass::CallNameWithoutParentheses
            | RakuAstClass::TypeSimple
            | RakuAstClass::TypeSetting
            // A package declaration does `Lookup` too, but nothing resolves it.
            | RakuAstClass::Class
            | RakuAstClass::Role
            | RakuAstClass::Grammar
            | RakuAstClass::Module
            | RakuAstClass::Package
            | RakuAstClass::Native
    )
}

pub fn node_accessor(node: &RakuAstNode, method: &str) -> Option<Value> {
    if let Some(child) = regex_extension::nested_accessor(node, method) {
        return Some(child);
    }
    // A statement records where it began, and so does a node of a family the
    // parser builds in its RakuAST shape (ADR-10723 Stage 2); every other node
    // (and a hand-built one) has no origin, which reads as `Nil` so `with`
    // skips it.
    if method == "origin" {
        return Some(origin::origin_of(node).unwrap_or(Value::NIL));
    }
    // A lookup's resolution, added by [`resolve`] to the unit tree `$*CU`
    // answers (and absent from a tree built any other way, which resolves
    // nothing).
    if method == "compile-time-value"
        && matches!(
            node.class,
            RakuAstClass::VarDeclarationSimple | RakuAstClass::ParameterTargetVar
        )
    {
        return Some(resolve::declared_type(node));
    }
    if is_resolvable_lookup(node.class) {
        match method {
            "is-resolved" => return Some(Value::truth(node.origin.2.is_some())),
            "resolution" => return Some(node.origin.2.clone().unwrap_or(Value::NIL)),
            _ => {}
        }
    }
    for f in &node.fields {
        if f.name == Some(method) && !is_unvisited_field(node, f) {
            return Some(field_to_value(&f.value));
        }
    }
    // Methods rakudo computes from the attributes (`default-scope`, `scope`
    // without a written scope, `Name.is-anonymous`, `Call::Method.dispatch`).
    if let Some(value) = derived_methods::call(node, method) {
        return Some(value);
    }
    // This inherited Block capability defaults to false and is not one of
    // the class's local model fields reported by reflection.
    if node.class == RakuAstClass::Block && method == "may-have-signature" {
        return Some(Value::truth(false));
    }
    // `code-statements` (undocumented in rakudo; `static::CLI`'s analysis
    // reads it, #15608) answers the same statements as `statements` for every
    // statement kind measured under `raku` (expressions, declarations, `use`,
    // phasers, loops, `CATCH`, empty statements).
    if (method == "statements"
        && matches!(
            node.class,
            RakuAstClass::StatementList | RakuAstClass::StatementSequence | RakuAstClass::SemiList
        ))
        || (method == "code-statements" && node.class == RakuAstClass::StatementList)
        || (method == "args" && node.class == RakuAstClass::ArgList)
    {
        let items = node
            .fields
            .iter()
            .filter(|f| !origin::is_origin(f))
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
            | RakuAstClass::RegexConjunction
            | RakuAstClass::RegexSequentialConjunction
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
    // `VarDeclaration::Simple.name`: the full spelling, sigil, twigil and
    // `desigilname` together (`$x`, `$*x`, `@!a`), as rakudo derives it.
    if method == "name"
        && node.class == RakuAstClass::VarDeclarationSimple
        && let Ok(sigil) = lower::leaf_str(node, "sigil")
        && let Some(RakuAstFieldValue::Node(desigil)) = node
            .fields
            .iter()
            .find(|f| f.name == Some("desigilname"))
            .map(|f| &f.value)
        && let Some(desigil) = lower::rakuast_node_of(desigil)
        && let Some(name_parts::NameShape::Identifier(desigil)) = name_parts::name_shape(desigil)
    {
        let twigil = lower::leaf_str(node, "twigil").unwrap_or_default();
        return Some(Value::str(format!("{sigil}{twigil}{desigil}")));
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
    let (name, absent) = fields::model_fields(node.class)
        .iter()
        .chain(inherited_fields::for_class(node.class))
        .find(|(name, _)| *name == method)?;
    match absent {
        fields::Absent::EmptyNode(class) => Some(empty_fields::empty_node(node, name, *class)),
        _ => absent.value(),
    }
}

/// Native methods currently exposed directly by a RakuAST model class.
///
/// This intentionally describes mutsu's implemented model API rather than
/// copying Rakudo's compiler-internal `IMPL-*` methods.  The result feeds
/// `.^methods(:local)`, so callers can discover constructors and accessors
/// without the RakuAST classes having ordinary registry entries.
pub fn local_method_names(class_name: &str) -> Option<Vec<&'static str>> {
    if let Some(names) = inherited_fields::local_names(class_name) {
        return Some(names);
    }
    // The core declarations a resolved lookup answers (see [`resolve`]).
    if matches!(
        class_name,
        "RakuAST::Declaration::External::Setting"
            | "RakuAST::Declaration::External::Constant"
            | "RakuAST::Declaration::Import"
            | "RakuAST::Declaration::LexicalPackage"
    ) {
        return Some(vec!["compile-time-value"]);
    }
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
    // The accessor over a class's single positional field (`Infix.operator`).
    if let Some(accessor) = fields::positional_accessor(class)
        && !names.contains(&accessor)
    {
        names.push(accessor);
    }
    names.extend(derived_methods::names(class));
    if class == RakuAstClass::CompUnit {
        names.push("add-check-code");
        names.push("add-sorry");
        names.push("add-worry");
        names.push("replace-statement-list");
    }
    if class == RakuAstClass::StatementList {
        names.push("code-statements");
    }
    if matches!(
        class,
        RakuAstClass::VarDeclarationSimple
            | RakuAstClass::ParameterTargetVar
            | RakuAstClass::Sub
            | RakuAstClass::Method
            | RakuAstClass::Submethod
    ) {
        names.push("compile-time-value");
    }
    if is_resolvable_lookup(class) {
        names.push("is-resolved");
        names.push("resolution");
    }
    if class == RakuAstClass::Parameter {
        names.push("meta-object");
    }
    if class == RakuAstClass::StatementExpression {
        names.push("set-expression");
    }
    if class == RakuAstClass::ArgList {
        names.push("push");
    }
    if matches!(
        class,
        RakuAstClass::ApplyInfix | RakuAstClass::ApplyListInfix
    ) {
        names.push("add-colonpair");
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
    if let Some(names) = inherited_fields::local_names(class_name) {
        return Some(names);
    }
    class_from_name(class_name).map(accessor_names)
}

fn class_from_name(class_name: &str) -> Option<RakuAstClass> {
    RAKUAST_CLASSES
        .iter()
        .copied()
        .find(|class| class.printed_name() == class_name)
}

fn constructor_is_supported(class: RakuAstClass) -> bool {
    if matches!(class, RakuAstClass::Module | RakuAstClass::Package) {
        return true;
    }
    if control_signature::constructor_schema(class.printed_name()).is_some() {
        return true;
    }
    if named_ctor::is_supported(class) {
        return true;
    }
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
            | RakuAstClass::VarCompilerBlock
            | RakuAstClass::VarCompilerRoutine
            | RakuAstClass::VarCompilerLookup
            | RakuAstClass::VarCompilerFile
            | RakuAstClass::VarCompilerLine
            | RakuAstClass::VarPackage
            | RakuAstClass::VarDynamic
            | RakuAstClass::StatementExpression
            | RakuAstClass::Label
            | RakuAstClass::StatementEmpty
            | RakuAstClass::StatementUse
            | RakuAstClass::StatementRequire
            | RakuAstClass::ApplyInfix
            | RakuAstClass::ApplyPrefix
            | RakuAstClass::ApplyPostfix
            | RakuAstClass::PostcircumfixLiteralHashIndex
            | RakuAstClass::PostcircumfixArrayIndex
            | RakuAstClass::PostcircumfixHashIndex
            | RakuAstClass::FunctionInfix
            | RakuAstClass::Postfix
            | RakuAstClass::MetaInfixAssign
            | RakuAstClass::MetaInfixNegate
            | RakuAstClass::BracketedInfix
            | RakuAstClass::RegexSequentialConjunction
            | RakuAstClass::RegexConjunction
            | RakuAstClass::RegexBacktrackModifiedAtom
            | RakuAstClass::RegexQuantifierBlockRange
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
            | RakuAstClass::CallBlockMethod
            | RakuAstClass::CallTermAsMethod
            | RakuAstClass::FlipFlop
            | RakuAstClass::Feed
            | RakuAstClass::StatementPrefixEager
            | RakuAstClass::TermCapture
            | RakuAstClass::QuoteWordsAtom
            | RakuAstClass::MetaInfixReverse
            | RakuAstClass::MetaInfixCross
            | RakuAstClass::MetaInfixZip
            | RakuAstClass::Block
            | RakuAstClass::PointyBlock
            | RakuAstClass::Blockoid
            | RakuAstClass::CircumfixParentheses
            | RakuAstClass::CircumfixArrayComposer
            | RakuAstClass::VarDeclarationPlaceholderPositional
            | RakuAstClass::VarDeclarationPlaceholderNamed
            | RakuAstClass::VarDeclarationPlaceholderSlurpyArray
            | RakuAstClass::VarDeclarationPlaceholderSlurpyHash
            | RakuAstClass::Sub
            | RakuAstClass::Method
            | RakuAstClass::Submethod
            | RakuAstClass::Signature
            | RakuAstClass::TraitReturns
            | RakuAstClass::TraitOf
            | RakuAstClass::Parameter
            | RakuAstClass::ParameterTargetVar
            | RakuAstClass::ParameterTargetTerm
            | RakuAstClass::VarDeclarationSimple
            | RakuAstClass::VarDeclarationSignature
            | RakuAstClass::InitializerAssign
            | RakuAstClass::InitializerCallAssign
            | RakuAstClass::InitializerBind
            | RakuAstClass::TypeSimple
            | RakuAstClass::TypeEnum
            | RakuAstClass::TypeDefinedness
            | RakuAstClass::TypeAnyDefinedness
            | RakuAstClass::TypeCoercion
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
            | RakuAstClass::Role
            | RakuAstClass::RoleBody
            | RakuAstClass::NamePartExpression
            | RakuAstClass::NamePartEmpty
            | RakuAstClass::NamePartEmptyEdge
            | RakuAstClass::TermName
            | RakuAstClass::TermNamed
            | RakuAstClass::TermTopicCall
            | RakuAstClass::TermWhatever
            | RakuAstClass::TermEmptySet
            | RakuAstClass::TermHyperWhatever
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

pub(crate) use assign_ro::{assignment_ro_expr, literal_assign_ro_expr};
pub(crate) use assign_target::{assign_to_target_expr, unwrap_grouped_lvalue};
pub(crate) use assign_writeback::{
    callable_lvalue_assign_expr, method_lvalue_assign_expr, method_lvalue_call,
    method_lvalue_call_parts, method_lvalue_roundtrip_assign_expr, named_sub_lvalue_assign_expr,
    subscript_adverb_lvalue_assign_expr,
};
pub(crate) use hash_literal::object_hash_expr;
pub(crate) use hyper_prefix::hyper_prefix_expr;
pub(crate) use word_list::{word_list_expr, words_atom_expr};
