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
    /// `False`, for a `Bool` flag the renderer elides when it is false.
    False,
    /// `0`, for an `int` flag (rakudo's `Pragma.off`).
    Zero,
    /// An empty node of the given class. A field rakudo always fills but whose
    /// *empty* value its renderer elides — `sub f { }` gists with no
    /// `signature`, yet `.signature` answers a defined, parameterless
    /// `RakuAST::Signature`.
    EmptyNode(RakuAstClass),
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
            Absent::False => Value::truth(false),
            Absent::Zero => Value::int(0),
            Absent::EmptyNode(class) => Value::rakuast(Box::new(super::RakuAstNode {
                class,
                fields: Vec::new(),
            })),
            Absent::Required => return None,
        })
    }
}

const EXPRESSION: Absent = Absent::TypeObject("RakuAST::Expression");
const BLOCK: Absent = Absent::TypeObject("RakuAST::Block");

/// The fields a RakuAST class declares, in rakudo's own declaration order
/// (which is the order `.^attributes` reports and the renderer emits).
pub(super) fn model_fields(class: RakuAstClass) -> &'static [(&'static str, Absent)] {
    use RakuAstClass::*;
    match class {
        StatementList => &[("statements", Absent::EmptyList)],
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
        IntLiteral | RatLiteral | StrLiteral => &[("value", Absent::Required)],
        VarLexical => &[("name", Absent::Required)],
        ApplyInfix => &[
            ("left", Absent::Required),
            ("infix", Absent::Required),
            ("right", Absent::Required),
        ],
        FunctionInfix => &[("function", Absent::Required)],
        ApplyPrefix => &[("prefix", Absent::Required), ("operand", Absent::Required)],
        ApplyPostfix => &[("operand", Absent::Required), ("postfix", Absent::Required)],
        Postfix => &[("operator", Absent::Required)],
        Block => &[("body", Absent::TypeObject("RakuAST::Blockoid"))],
        Blockoid => &[("statement-list", Absent::Required)],
        Sub => &[
            ("name", Absent::TypeObject("RakuAST::Name")),
            ("signature", Absent::EmptyNode(Signature)),
            ("traits", Absent::EmptyList),
            ("body", Absent::Required),
        ],
        Signature => &[
            ("parameters", Absent::EmptyList),
            ("returns", Absent::TypeObject("RakuAST::Node")),
        ],
        MetaInfixAssign => &[("infix", Absent::Required)],
        MetaInfixHyper => &[
            ("dwim-left", Absent::False),
            ("infix", Absent::Required),
            ("dwim-right", Absent::False),
        ],
        TraitReturns | TraitOf => &[("type", Absent::Required)],
        Parameter => &[
            ("type", Absent::TypeObject("RakuAST::Type")),
            ("names", Absent::EmptyList),
            ("type-captures", Absent::EmptyList),
            ("target", Absent::TypeObject("RakuAST::ParameterTarget")),
            // Tri-state on rakudo: `False` on a plain positional, but left
            // UNSET (an undefined `Bool`) when optionality follows from
            // something else — a default, a slurpy, a named parameter.
            ("optional", Absent::TypeObject("Bool")),
            ("default", EXPRESSION),
            ("where", EXPRESSION),
            ("slurpy", Absent::TypeObject("RakuAST::Parameter::Slurpy")),
            ("sub-signature", Absent::TypeObject("RakuAST::Signature")),
        ],
        ParameterTargetVar => &[("name", Absent::Required)],
        VarDeclarationSimple => &[
            ("sigil", Absent::Required),
            ("desigilname", Absent::Required),
            ("initializer", Absent::TypeObject("RakuAST::Initializer")),
        ],
        InitializerAssign => &[("expression", Absent::Required)],
        TypeSimple | TypeSetting | TypeCapture => &[("name", Absent::Required)],
        TypeEnum => &[("name", Absent::Required), ("term", Absent::Required)],
        QuotedRegex => &[
            ("match-immediately", Absent::False),
            ("body", Absent::Required),
            ("adverbs", Absent::EmptyList),
        ],
        RegexSequence | RegexAlternation => &[("terms", Absent::EmptyList)],
        RegexLiteral => &[("text", Absent::Required)],
        RegexQuote => &[("quoted", Absent::Required)],
        RegexGroup | RegexCapturingGroup | RegexWithWhitespace => &[("regex", Absent::Required)],
        RegexNamedCapture => &[
            ("name", Absent::Required),
            ("array", Absent::False),
            ("regex", Absent::Required),
        ],
        RegexInterpolation => &[("sequential", Absent::False), ("var", Absent::Required)],
        RegexQuantifiedAtom => &[
            ("atom", Absent::Required),
            ("quantifier", Absent::Required),
            ("separator", Absent::TypeObject("RakuAST::Regex::Term")),
            ("trailing-separator", Absent::False),
        ],
        RegexDeclaration | TokenDeclaration | RuleDeclaration => {
            &[("name", Absent::Required), ("body", Absent::Required)]
        }
        Grammar => &[("name", Absent::Required), ("body", Absent::Required)],
        Pragma => &[
            ("name", Absent::Required),
            ("argument", EXPRESSION),
            ("off", Absent::Zero),
        ],
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
        StatementUnless | StatementWithout => {
            &[("condition", Absent::Required), ("body", Absent::Required)]
        }
        StatementElsif | StatementOrwith => {
            &[("condition", Absent::Required), ("then", Absent::Required)]
        }
        // Every statement modifier exposes the condition/topic it was written
        // with as `.expression`, which mutsu stores as the node's single
        // positional field.
        StatementModifierGiven
        | StatementModifierIf
        | StatementModifierUnless
        | StatementModifierWith
        | StatementModifierWithout => &[("expression", Absent::Required)],
        _ => &[],
    }
}

/// The accessor a class exposes over its single positional field. The named
/// lookup runs first, so a class with a *named* field of the same name (e.g.
/// `Call::Name.name`) is unaffected.
pub(super) fn positional_accessor(class: RakuAstClass) -> Option<&'static str> {
    use RakuAstClass::*;
    Some(match class {
        IntLiteral | RatLiteral | StrLiteral => "value",
        FunctionInfix => "function",
        VarLexical => "name",
        Blockoid => "statement-list",
        InitializerAssign => "expression",
        MetaInfixAssign => "infix",
        TypeSimple | TypeSetting | TypeCapture => "name",
        TraitReturns | TraitOf => "type",
        RegexLiteral => "text",
        RegexQuote => "quoted",
        RegexSequence | RegexAlternation => "terms",
        RegexGroup | RegexCapturingGroup | RegexWithWhitespace => "regex",
        StatementModifierGiven
        | StatementModifierIf
        | StatementModifierUnless
        | StatementModifierWith
        | StatementModifierWithout => "expression",
        _ => return None,
    })
}
