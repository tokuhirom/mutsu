//! The model fields each RakuAST class declares, and what an accessor answers
//! when the node does not carry one.
//!
//! A RakuAST node only stores the fields its source actually produced — the
//! renderer elides an absent `else`, an empty `elsifs`, a false `dwim-left`.
//! Rakudo, by contrast, declares every field as an attribute, so asking for one
//! that is not there is legal and answers with an undefined (or empty, or
//! false) value; that is what makes `.defined` the natural way to test for an
//! optional clause:
//!
//! ```text
//! $ raku -e 'say Q[say 1 with 2].AST.statements[0].loop-modifier.defined'
//! False
//! ```
//!
//! So the field list here is the *declaration*, separate from the fields a
//! given node happens to hold: [`node_accessor`](super::node_accessor) consults
//! it when its lookup over the node's own fields misses, and
//! `.^attributes(:local)` / `.^methods(:local)` report it so introspection and
//! dispatch cannot disagree about which accessors exist.
//!
//! Every [`Absent`] answer below was measured against rakudo 2026.07 — both the
//! value (`.^attributes` reports the declared type; an unset attribute holds
//! that type's type object) and the shape (`List` answers `()`, never
//! undefined; a `Bool` flag answers `False`; an `int` flag answers `0`).

use super::RakuAstClass;
pub(super) use super::positional_fields::positional_accessor;
use crate::symbol::Symbol;
use crate::value::Value;

/// What a declared model field answers with when a node does not carry it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Absent {
    /// An undefined type object of the field's declared type — what rakudo's
    /// unset attribute holds (`.else` on an `if` with no else is `(Block)`).
    TypeObject(&'static str),
    /// An empty list. A `List`-typed field is never undefined in rakudo: an
    /// `if` with no `elsif` answers `()`, not a type object.
    EmptyList,
    /// An empty `Map`, for a `Map`-typed field (`Doc::Block.config`).
    EmptyMap,
    /// `False`, for a `Bool` flag the renderer elides when it is false.
    False,
    /// `0`, for an `int` flag (rakudo's `Pragma.off`).
    Zero,
    /// An empty node of the given class. A field rakudo always fills but whose
    /// *empty* value its renderer elides — `sub f { }` gists with no
    /// `signature`, yet `.signature` answers a defined, parameterless
    /// `RakuAST::Signature`.
    EmptyNode(RakuAstClass),
    /// A string the field holds unless given (`InternalModifier.modifier` is
    /// the short spelling `"i"` when the source wrote `:i`).
    Str(&'static str),
    /// Structurally required: a well-formed node always carries it, so there is
    /// no absent value to invent. Asking for it on a malformed node falls
    /// through to ordinary "no such method" dispatch rather than answering with
    /// a fiction.
    Required,
}

impl Absent {
    /// The value the accessor answers with, or `None` for a required field.
    pub(super) fn value(self) -> Option<Value> {
        Some(match self {
            Absent::TypeObject(name) => Value::package(Symbol::intern(name)),
            Absent::EmptyList => Value::array(Vec::new()),
            Absent::EmptyMap => Value::hash(crate::value::ValueMap::default()),
            Absent::False => Value::truth(false),
            Absent::Zero => Value::int(0),
            Absent::Str(s) => Value::str(s.to_string()),
            Absent::EmptyNode(class) => Value::rakuast(Box::new(super::RakuAstNode {
                origin: Default::default(),
                class,
                fields: Vec::new(),
            })),
            Absent::Required => return None,
        })
    }
}

const EXPRESSION: Absent = Absent::TypeObject("RakuAST::Expression");
const BLOCK: Absent = Absent::TypeObject("RakuAST::Block");
const NAME: Absent = Absent::TypeObject("RakuAST::Name");
const BACKTRACK: Absent = Absent::TypeObject("RakuAST::Regex::Backtrack");

/// The fields a RakuAST class declares, in rakudo's own declaration order
/// (which is the order `.^attributes` reports and the renderer emits).
pub(super) fn model_fields(class: RakuAstClass) -> &'static [(&'static str, Absent)] {
    use RakuAstClass::*;
    match class {
        CompUnit => &[
            ("statement-list", Absent::Required),
            ("comp-unit-name", Absent::Required),
        ],
        RegexCharClass(kind) => kind.model_fields(),
        RegexAssertionCharClass => &[("elements", Absent::EmptyList)],
        RegexCharClassElementEnumeration => {
            &[("elements", Absent::EmptyList), ("negated", Absent::False)]
        }
        RegexCharClassElementRule => &[("name", Absent::Required), ("negated", Absent::False)],
        RegexCharClassElementProperty => &[
            ("property", Absent::Required),
            ("inverted", Absent::False),
            ("predicate", Absent::TypeObject("RakuAST::Expression")),
            ("negated", Absent::False),
        ],
        RegexCharClassEnumerationElementCharacter => &[("character", Absent::Required)],
        RegexCharClassEnumerationElementRange => {
            &[("from", Absent::Required), ("to", Absent::Required)]
        }
        RegexInternalModifierIgnoreCase => {
            &[("modifier", Absent::Str("i")), ("negated", Absent::False)]
        }
        RegexInternalModifierIgnoreMark => {
            &[("modifier", Absent::Str("m")), ("negated", Absent::False)]
        }
        RegexInternalModifierSigspace => {
            &[("modifier", Absent::Str("s")), ("negated", Absent::False)]
        }
        RegexInternalModifierRatchet => {
            &[("modifier", Absent::Str("r")), ("negated", Absent::False)]
        }
        RegexQuantifierZeroOrMore | RegexQuantifierOneOrMore | RegexQuantifierZeroOrOne => {
            &[("backtrack", BACKTRACK)]
        }
        RegexBacktrackModifiedAtom => &[("atom", Absent::Required), ("backtrack", BACKTRACK)],
        RegexQuantifierBlockRange => &[("block", Absent::Required), ("backtrack", BACKTRACK)],
        RegexQuantifierRange => &[
            ("min", Absent::TypeObject("Int")),
            ("max", Absent::TypeObject("Int")),
            ("excludes-min", Absent::False),
            ("excludes-max", Absent::False),
            ("backtrack", BACKTRACK),
        ],
        StatementList => &[("statements", Absent::EmptyList)],
        StatementAlso => &[("traits", Absent::EmptyList)],
        StatementWhenever => &[("trigger", Absent::Required), ("body", Absent::Required)],
        StatementExpression => &[
            ("expression", Absent::Required),
            (
                "condition-modifier",
                Absent::TypeObject("RakuAST::StatementModifier::Condition"),
            ),
            (
                "loop-modifier",
                Absent::TypeObject("RakuAST::StatementModifier::Loop"),
            ),
        ],
        IntLiteral | NumLiteral | RatLiteral | VersionLiteral | Constant | Literal
        | ComplexLiteral | StrLiteral => &[("value", Absent::Required)],
        VarLexical | VarDynamic | VarDoc => &[("name", Absent::Required)],
        VarCompilerLookup => &[("name", Absent::Required)],
        VarCompilerFile => &[("file", Absent::Required)],
        VarCompilerLine => &[("line", Absent::Required)],
        QuotedString => &[
            ("segments", Absent::Required),
            ("processors", Absent::EmptyList),
        ],
        VarPackage => &[("name", Absent::Required), ("sigil", Absent::Required)],
        Name => &[
            ("parts", Absent::EmptyList),
            ("colonpairs", Absent::EmptyList),
        ],
        NamePartSimple => &[("name", Absent::Required)],
        NamePartExpression => &[("expr", Absent::Required)],
        TermName => &[("name", Absent::Required)],
        TermNamed => &[("name", Absent::Required)],
        TermTopicCall => &[("call", Absent::Required)],
        CallName
        | CallNameWithoutParentheses
        | CallMethod
        | CallPrivateMethod
        | CallNameAsMethod
        | CallMetaMethod => &[
            ("name", Absent::Required),
            ("args", Absent::EmptyNode(ArgList)),
        ],
        CallTermAsMethod => &[
            ("callee", Absent::Required),
            ("args", Absent::EmptyNode(ArgList)),
        ],
        CallBlockMethod => &[
            ("block", Absent::Required),
            ("args", Absent::EmptyNode(ArgList)),
        ],
        CallTerm => &[("args", Absent::EmptyNode(ArgList))],
        VarAttributePublic => &[
            ("name", Absent::Required),
            ("args", Absent::EmptyNode(ArgList)),
        ],
        ApplyInfix => &[
            ("left", Absent::Required),
            ("infix", Absent::Required),
            ("right", Absent::Required),
        ],
        FunctionInfix => &[("function", Absent::Required)],
        ApplyPrefix => &[("prefix", Absent::Required), ("operand", Absent::Required)],
        ApplyPostfix => &[("operand", Absent::Required), ("postfix", Absent::Required)],
        PostcircumfixLiteralHashIndex | PostcircumfixArrayIndex => &[
            ("index", Absent::Required),
            ("colonpairs", Absent::EmptyList),
            ("assignee", EXPRESSION),
        ],
        PostcircumfixHashIndex => &[
            ("index", Absent::Required),
            ("colonpairs", Absent::EmptyList),
        ],
        Postfix => &[("operator", Absent::Required)],
        Block => &[
            ("body", Absent::TypeObject("RakuAST::Blockoid")),
            ("implicit-topic", Absent::TypeObject("Bool")),
            ("required-topic", Absent::TypeObject("Bool")),
            ("exception", Absent::TypeObject("Bool")),
        ],
        PointyBlock => &[
            ("signature", Absent::EmptyNode(Signature)),
            ("body", Absent::TypeObject("RakuAST::Blockoid")),
            ("implicit-topic", Absent::TypeObject("Bool")),
            ("required-topic", Absent::TypeObject("Bool")),
            ("exception", Absent::TypeObject("Bool")),
        ],
        // `through-pragma` is set when a `use :D` default made the type definite;
        // a smiley the source wrote leaves it False.
        TypeDefinedness => &[("through-pragma", Absent::False)],
        Blockoid => &[("statement-list", Absent::Required)],
        VarDeclarationPlaceholderPositional | VarDeclarationPlaceholderNamed => {
            &[("lexical-name", Absent::Required)]
        }
        VarDeclarationPlaceholderSlurpyArray => &[],
        VarDeclarationPlaceholderSlurpyHash => &[],
        StubFail | StubDie | StubWarn => &[("args", Absent::EmptyNode(ArgList))],
        Sub => &[
            ("scope", Absent::Required),
            ("multiness", Absent::Str("")),
            ("name", Absent::TypeObject("RakuAST::Name")),
            ("signature", Absent::EmptyNode(Signature)),
            ("traits", Absent::EmptyList),
            ("body", Absent::Required),
        ],
        Method | Submethod => &[
            ("scope", Absent::Required),
            ("multiness", Absent::Str("")),
            ("private", Absent::False),
            ("name", Absent::TypeObject("RakuAST::Name")),
            ("signature", Absent::EmptyNode(Signature)),
            ("traits", Absent::EmptyList),
            ("body", Absent::Required),
        ],
        Signature => &[
            ("parameters", Absent::EmptyList),
            ("returns", Absent::TypeObject("RakuAST::Node")),
            ("is-array", Absent::False),
        ],
        MetaInfixAssign => &[("infix", Absent::Required)],
        MetaInfixNegate | BracketedInfix => &[("infix", Absent::Required)],
        RegexSequentialConjunction => &[("branches", Absent::Required)],
        RegexConjunction => &[("branches", Absent::Required)],
        RegexBackReferencePositional => &[("index", Absent::Required)],
        RegexBackReferenceNamed => &[("name", Absent::Required)],
        RegexStatement => &[("statement", Absent::Required)],
        RegexNested => &[("goal", Absent::Required), ("expr", Absent::Required)],
        VarPositionalCapture => &[("index", Absent::Required)],
        VarNamedCapture => &[
            ("index", Absent::Required),
            ("sigil", Absent::Required),
            ("colonpairs", Absent::EmptyList),
        ],
        MetaPostfixHyper => &[("postfix", Absent::Required)],
        MetaPrefixHyper => &[("prefix", Absent::Required)],
        TermCapture => &[("source", Absent::Required)],
        QuoteWordsAtom => &[("atom", Absent::Required)],
        MetaInfixReverse => &[("infix", Absent::Required)],
        MetaInfixCross => &[("infix", Absent::Required)],
        MetaInfixZip => &[("infix", Absent::Required)],
        MetaInfixHyper => &[
            ("dwim-left", Absent::False),
            ("infix", Absent::Required),
            ("dwim-right", Absent::False),
        ],
        TraitReturns | TraitOf | TraitDoes | TraitHides => &[("type", Absent::Required)],
        StatementTrusts | TraitTrusts => &[("type", Absent::Required)],
        TraitHandles => &[("term", Absent::Required)],
        TraitWill => &[("phase", Absent::Required), ("block", Absent::Required)],
        TraitIs => &[
            ("name", Absent::TypeObject("RakuAST::Name")),
            ("argument", Absent::TypeObject("RakuAST::Expression")),
            ("type", Absent::TypeObject("RakuAST::Type")),
        ],
        Parameter => &[
            ("default-rw", Absent::False),
            ("type", Absent::TypeObject("RakuAST::Type")),
            ("names", Absent::EmptyList),
            ("type-captures", Absent::EmptyList),
            ("invocant", Absent::False),
            ("target", Absent::TypeObject("RakuAST::ParameterTarget")),
            // Tri-state on rakudo: `False` on a plain positional, but left
            // UNSET (an undefined `Bool`) when optionality follows from
            // something else — a default, a slurpy, a named parameter.
            ("optional", Absent::TypeObject("Bool")),
            ("default", EXPRESSION),
            ("where", EXPRESSION),
            ("array-shape", Absent::TypeObject("RakuAST::Statement")),
            ("slurpy", Absent::TypeObject("RakuAST::Parameter::Slurpy")),
            ("sub-signature", Absent::TypeObject("RakuAST::Signature")),
            ("traits", Absent::EmptyList),
        ],
        ParameterTargetVar | ParameterTargetTerm => &[("name", Absent::Required)],
        VarDeclarationSimple => &[
            ("type", Absent::TypeObject("RakuAST::Type")),
            ("shape", Absent::TypeObject("RakuAST::SemiList")),
            ("sigil", Absent::Required),
            ("desigilname", Absent::Required),
            ("traits", Absent::EmptyList),
            ("initializer", Absent::TypeObject("RakuAST::Initializer")),
            // Rakudo answers `my` for a declaration with no scope, an empty
            // string for no twigil and an undefined expression for no `where`.
            ("twigil", Absent::Str("")),
            ("where", EXPRESSION),
            ("scope", Absent::Str("my")),
        ],
        // Rakudo answers `our` for a constant with no scope (measured on 2026.09).
        VarDeclarationConstant => &[
            ("type", Absent::TypeObject("RakuAST::Type")),
            ("name", Absent::Required),
            ("traits", Absent::EmptyList),
            ("initializer", Absent::TypeObject("RakuAST::Initializer")),
            ("scope", Absent::Str("our")),
        ],
        VarDeclarationAnonymous => &[
            ("type", Absent::TypeObject("RakuAST::Type")),
            ("scope", Absent::Str("my")),
            ("sigil", Absent::Required),
            ("traits", Absent::EmptyList),
            ("twigil", Absent::Str("")),
            ("shape", Absent::TypeObject("RakuAST::SemiList")),
            ("where", EXPRESSION),
            ("initializer", Absent::TypeObject("RakuAST::Initializer")),
        ],
        InitializerAssign | InitializerBind => &[("expression", Absent::Required)],
        InitializerCallAssign => &[("postfixish", Absent::Required)],
        VarDeclarationSignature => &[
            ("signature", Absent::Required),
            ("initializer", Absent::TypeObject("RakuAST::Initializer")),
        ],
        // A subset without a `where` clause answers an undefined expression.
        TypeSubset => &[("where", EXPRESSION)],
        // `is repr(...)` is read back off the declaration; absent it is an undefined Str.
        Native => &[("repr", Absent::TypeObject("Str"))],
        TypeSimple | TypeSetting | TypeCapture => &[("name", Absent::Required)],
        // `R[]` has no `args` field in rakudo (the renderer elides the empty list).
        TypeParameterized => &[
            ("base-type", Absent::Required),
            ("args", Absent::EmptyNode(RakuAstClass::ArgList)),
        ],
        TypeEnum => &[
            ("name", Absent::Required),
            ("of", Absent::TypeObject("RakuAST::Type")),
            ("term", Absent::Required),
            ("traits", Absent::EmptyList),
        ],
        QuotedRegex | QuotedRegexRx => &[
            ("match-immediately", Absent::False),
            ("body", Absent::Required),
            ("adverbs", Absent::EmptyList),
        ],
        RegexSequence => &[("terms", Absent::EmptyList)],
        RegexAlternation | RegexSequentialAlternation => &[("branches", Absent::EmptyList)],
        RegexLiteral => &[("text", Absent::Required)],
        RegexQuote => &[("quoted", Absent::Required)],
        RegexGroup | RegexCapturingGroup | RegexWithWhitespace => &[("regex", Absent::Required)],
        RegexBlock => &[("block", Absent::Required)],
        RegexNamedCapture => &[
            ("name", Absent::Required),
            ("array", Absent::False),
            ("hash", Absent::False),
            ("regex", Absent::Required),
        ],
        RegexAssertionNamed => &[("name", Absent::Required), ("capturing", Absent::False)],
        RegexAssertionNamedArgs => &[
            ("name", Absent::Required),
            ("args", Absent::EmptyNode(RakuAstClass::ArgList)),
            ("capturing", Absent::False),
        ],
        RegexAssertionAlias => &[("name", Absent::Required), ("assertion", Absent::Required)],
        RegexAssertionNamedRegexArg => {
            &[("name", Absent::Required), ("regex-arg", Absent::Required)]
        }
        RegexAssertionLookahead => &[("negated", Absent::False), ("assertion", Absent::Required)],
        RegexAssertionInterpolatedVar => {
            &[("sequential", Absent::False), ("var", Absent::Required)]
        }
        RegexAssertionCallable => &[
            ("callee", Absent::Required),
            ("args", Absent::TypeObject("RakuAST::ArgList")),
        ],
        RegexAssertionPredicateBlock => &[("negated", Absent::False), ("block", Absent::Required)],
        RegexAssertionInterpolatedBlock => {
            &[("block", Absent::Required), ("sequential", Absent::False)]
        }
        RegexInterpolation => &[("sequential", Absent::False), ("var", Absent::Required)],
        DocMarkup => &[
            ("letter", Absent::Required),
            ("opener", Absent::Str("<")),
            ("closer", Absent::Str(">")),
            ("atoms", Absent::EmptyList),
            ("meta", Absent::EmptyList),
        ],
        DocParagraph => &[("atoms", Absent::EmptyList)],
        DocLegacyRow => &[
            ("column-dividers", Absent::Required),
            ("column-offsets", Absent::EmptyList),
            ("cells", Absent::EmptyList),
        ],
        DocDeclarator => &[
            ("WHEREFORE", Absent::TypeObject("NQPMu")),
            ("leading", Absent::EmptyList),
            ("trailing", Absent::EmptyList),
        ],
        DocBlock => &[
            ("margin", Absent::Str("")),
            ("type", Absent::Required),
            ("level", Absent::Str("")),
            ("directive", Absent::False),
            ("abbreviated", Absent::False),
            ("for", Absent::False),
            ("config", Absent::EmptyMap),
            ("paragraphs", Absent::EmptyList),
        ],
        ColonPairVariable | ColonPairValue | ColonPairNumber => {
            &[("key", Absent::Required), ("value", Absent::Required)]
        }
        Substitution => &[
            ("immutable", Absent::False),
            ("samespace", Absent::False),
            ("adverbs", Absent::EmptyList),
            ("infix", Absent::TypeObject("RakuAST::Infixish")),
            ("pattern", Absent::Required),
            ("replacement", Absent::Required),
        ],
        Transliteration => &[
            ("destructive", Absent::Required),
            ("left", Absent::Required),
            ("right", Absent::Required),
            ("adverbs", Absent::EmptyList),
        ],
        RegexQuantifiedAtom => &[
            ("atom", Absent::Required),
            ("quantifier", Absent::Required),
            ("separator", Absent::TypeObject("RakuAST::Regex::Term")),
            ("trailing-separator", Absent::False),
        ],
        RegexDeclaration | TokenDeclaration | RuleDeclaration => &[
            ("scope", Absent::Str("has")),
            ("multiness", Absent::Str("")),
            ("name", NAME),
            ("signature", Absent::EmptyNode(Signature)),
            ("traits", Absent::EmptyList),
            ("body", Absent::Required),
        ],
        // A package declaration without a source name (`class { }`) has no
        // `name`: its accessor answers the undefined `Name`, as rakudo's.
        // `parsed-declarator` is the keyword, set only for a registered
        // `EXPORTHOW::DECLARE` declarator (`declare_how`).
        Class => &[
            ("scope", Absent::Str("our")),
            ("parsed-declarator", Absent::Str("class")),
            ("name", NAME),
            ("repr", Absent::TypeObject("Str")),
            ("traits", Absent::EmptyList),
            ("body", Absent::Required),
        ],
        Role => &[
            ("scope", Absent::Str("our")),
            ("parsed-declarator", Absent::Str("role")),
            ("name", NAME),
            ("repr", Absent::TypeObject("Str")),
            ("traits", Absent::EmptyList),
            ("body", Absent::Required),
            ("parameterization", Absent::EmptyNode(Signature)),
        ],
        RoleBody => &[("body", Absent::Required)],
        Grammar => &[
            ("scope", Absent::Str("our")),
            ("parsed-declarator", Absent::Str("grammar")),
            ("name", NAME),
            ("repr", Absent::TypeObject("Str")),
            ("traits", Absent::EmptyList),
            ("body", Absent::Required),
        ],
        Module | Package => &[
            ("scope", Absent::Str("our")),
            ("name", NAME),
            ("repr", Absent::TypeObject("Str")),
            ("traits", Absent::EmptyList),
            ("body", Absent::Required),
        ],
        Pragma => &[
            ("name", Absent::Required),
            ("argument", EXPRESSION),
            ("off", Absent::Zero),
        ],
        StatementUse => &[("module-name", Absent::Required), ("argument", EXPRESSION)],
        StatementRequire => &[
            ("module-name", Absent::TypeObject("RakuAST::Name")),
            ("file", EXPRESSION),
            ("argument", EXPRESSION),
        ],
        StatementImport => &[("module-name", Absent::Required), ("argument", EXPRESSION)],
        StatementNeed => &[("module-names", Absent::EmptyList)],
        StatementLanguageVersion => &[("version", Absent::Required)],
        CircumfixHashComposer => &[("expression", EXPRESSION), ("object-hash", Absent::False)],
        // The conditional family. `If` and `With` take the `elsif`/`orwith`
        // chain and an `else`; `Unless` and `Without` take neither (rakudo
        // rejects them at compile time) and name their block `body` rather than
        // `then` — the asymmetry #8123 matched.
        StatementIf | StatementWith => &[
            ("condition", Absent::Required),
            ("then", Absent::Required),
            ("elsifs", Absent::EmptyList),
            ("else", BLOCK),
        ],
        // A loop's `setup`, `condition` and `increment` are optional
        // `RakuAST::Expression` attributes (measured on 2026.09); the
        // `while` / `until` / `repeat` forms always carry a `condition`.
        StatementLoop => &[
            ("setup", EXPRESSION),
            ("condition", EXPRESSION),
            ("increment", EXPRESSION),
        ],
        StatementLoopWhile
        | StatementLoopUntil
        | StatementLoopRepeatWhile
        | StatementLoopRepeatUntil => &[
            ("setup", EXPRESSION),
            ("condition", Absent::Required),
            ("increment", EXPRESSION),
        ],
        StatementUnless | StatementWithout => {
            &[("condition", Absent::Required), ("body", Absent::Required)]
        }
        StatementElsif | StatementOrwith => {
            &[("condition", Absent::Required), ("then", Absent::Required)]
        }
        // Every statement modifier exposes the condition/topic it was written
        // with as `.expression`, which mutsu stores as the node's single
        // positional field.
        StatementModifierFor
        | StatementModifierWhile
        | StatementModifierUntil
        | StatementModifierGiven
        | StatementModifierIf
        | StatementModifierUnless
        | StatementModifierWith
        | StatementModifierWhen
        | StatementModifierWithout => &[("expression", Absent::Required)],
        _ => &[],
    }
}
