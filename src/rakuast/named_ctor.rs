//! Named-argument constructors of the RakuAST classes whose `new` takes only
//! named arguments (ADR-10723: a tree built in Raku equals the parsed one).
//!
//! Each class lists its arguments in the order rakudo 2026.09's `.raku` prints
//! them, which is the order the read direction (`convert.rs`) stores the
//! fields in, so a constructed node renders and lowers exactly like a parsed
//! one. Which arguments are required, and the defaults rakudo fills in
//! (`Statement::For`'s `mode => "serial"`, `Term::Reduce`'s
//! `triangle => False`), were measured with `.^find_method('new').signature`
//! and `.raku` of a constructed node.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

/// What one named argument holds.
#[derive(Clone, Copy)]
enum Kind {
    /// A node (or a value the class stores as a node field, like a `Str`).
    Node,
    /// A list of nodes (`labels`, `module-names`, `traits`).
    List,
    /// A `Bool` flag stored only when true (`dwim-left`).
    Flag,
}

/// How a constructor treats an argument it was not given.
#[derive(Clone, Copy)]
enum Missing {
    Required,
    Omit,
    /// Store this string (`mode => "serial"`).
    DefaultStr(&'static str),
    /// Store `False` (`triangle => False`).
    DefaultFalse,
}

/// A string argument rakudo does not keep when it equals the class's
/// default (`VarDeclaration::Term`'s `scope => "my"`).
const DEFAULT_SCOPES: &[(&str, &str)] = &[("RakuAST::VarDeclaration::Term", "my")];

type Arg = (&'static str, Kind, Missing);

use Kind::{Flag, List, Node};
use Missing::{DefaultFalse, DefaultStr, Omit, Required};

const LABELS: Arg = ("labels", List, Omit);

/// The class a named constructor builds and its arguments, in `.raku` order.
// Cost: O(1).
fn schema(class_name: &str) -> Option<(RakuAstClass, &'static [Arg])> {
    use RakuAstClass as C;
    Some(match class_name {
        "RakuAST::Ternary" => (
            C::Ternary,
            &[
                ("condition", Node, Required),
                ("then", Node, Required),
                ("else", Node, Required),
            ],
        ),
        "RakuAST::FatArrow" => (
            C::FatArrow,
            &[("key", Node, Required), ("value", Node, Required)],
        ),
        "RakuAST::Call::MetaMethod" => (
            C::CallMetaMethod,
            &[("name", Node, Required), ("args", Node, Omit)],
        ),
        "RakuAST::Call::PrivateMethod" => (
            C::CallPrivateMethod,
            &[("name", Node, Required), ("args", Node, Omit)],
        ),
        "RakuAST::Call::QuotedMethod" => (
            C::CallQuotedMethod,
            &[
                ("name", Node, Required),
                ("args", Node, Omit),
                ("dispatch", Node, Omit),
            ],
        ),
        "RakuAST::Call::Term" => (C::CallTerm, &[("args", Node, Omit)]),
        "RakuAST::Call::NameAsMethod" => (
            C::CallNameAsMethod,
            &[
                ("name", Node, Required),
                ("args", Node, Omit),
                ("dispatch", Node, Omit),
            ],
        ),
        "RakuAST::Call::TermAsMethod" => (
            C::CallTermAsMethod,
            &[
                ("callee", Node, Required),
                ("args", Node, Omit),
                ("dispatch", Node, Omit),
            ],
        ),
        "RakuAST::Transliteration" => (
            C::Transliteration,
            &[
                ("destructive", Node, Required),
                ("left", Node, Required),
                ("right", Node, Required),
                ("adverbs", List, Omit),
            ],
        ),
        "RakuAST::Pragma" => (
            C::Pragma,
            &[
                ("off", Flag, Omit),
                ("name", Node, Required),
                ("argument", Node, Omit),
            ],
        ),
        "RakuAST::ColonPair::Number" => (
            C::ColonPairNumber,
            &[("key", Node, Required), ("value", Node, Omit)],
        ),
        "RakuAST::Substitution" => (
            C::Substitution,
            &[
                ("immutable", Node, DefaultFalse),
                ("samespace", Node, DefaultFalse),
                ("adverbs", List, Omit),
                ("infix", Node, Omit),
                ("pattern", Node, Required),
                ("replacement", Node, Required),
            ],
        ),
        "RakuAST::MetaInfix::Hyper" => (
            C::MetaInfixHyper,
            &[
                ("dwim-left", Flag, Omit),
                ("infix", Node, Required),
                ("dwim-right", Flag, Omit),
            ],
        ),
        "RakuAST::Statement::Catch" => (C::StatementCatch, &[("body", Node, Required)]),
        "RakuAST::Statement::Control" => (C::StatementControl, &[("body", Node, Required)]),
        "RakuAST::Statement::Default" => (C::StatementDefault, &[LABELS, ("body", Node, Required)]),
        "RakuAST::Statement::For" => (
            C::StatementFor,
            &[
                LABELS,
                ("mode", Node, DefaultStr("serial")),
                ("source", Node, Required),
                // The signature also takes `:$otherwise`, which rakudo
                // 2026.09 accepts and drops: `For` has no such attribute.
                ("body", Node, Required),
            ],
        ),
        "RakuAST::Statement::Given" => (
            C::StatementGiven,
            &[LABELS, ("source", Node, Required), ("body", Node, Required)],
        ),
        "RakuAST::Statement::When" => (
            C::StatementWhen,
            &[
                LABELS,
                ("condition", Node, Required),
                ("body", Node, Required),
            ],
        ),
        "RakuAST::Statement::Import" => (
            C::StatementImport,
            &[
                LABELS,
                ("module-name", Node, Required),
                ("argument", Node, Omit),
            ],
        ),
        "RakuAST::Statement::Need" => (
            C::StatementNeed,
            &[LABELS, ("module-names", List, Required)],
        ),
        "RakuAST::Statement::Loop" => (
            C::StatementLoop,
            &[
                LABELS,
                ("setup", Node, Omit),
                ("condition", Node, Omit),
                ("increment", Node, Omit),
                ("body", Node, Required),
            ],
        ),
        "RakuAST::Statement::Loop::RepeatUntil" => (
            C::StatementLoopRepeatUntil,
            &[LABELS, ("body", Node, Required), ("condition", Node, Omit)],
        ),
        "RakuAST::Statement::Loop::RepeatWhile" => (
            C::StatementLoopRepeatWhile,
            &[LABELS, ("body", Node, Required), ("condition", Node, Omit)],
        ),
        "RakuAST::Stub::Die" => (C::StubDie, &[("args", Node, Omit)]),
        "RakuAST::Stub::Fail" => (C::StubFail, &[("args", Node, Omit)]),
        "RakuAST::Stub::Warn" => (C::StubWarn, &[("args", Node, Omit)]),
        "RakuAST::Term::Reduce" => (
            C::TermReduce,
            &[
                ("triangle", Node, DefaultFalse),
                ("infix", Node, Required),
                ("args", Node, Required),
            ],
        ),
        "RakuAST::Term::Self" => (C::TermSelf, &[("variable", Node, Omit)]),
        "RakuAST::Type::Subset" => (
            C::TypeSubset,
            &[
                ("scope", Node, Omit),
                ("name", Node, Required),
                ("of", Node, Omit),
                ("where", Node, Omit),
                ("traits", List, Omit),
            ],
        ),
        "RakuAST::Var::Attribute::Public" => (
            C::VarAttributePublic,
            &[("name", Node, Required), ("args", Node, Omit)],
        ),
        "RakuAST::VarDeclaration::Constant" => (
            C::VarDeclarationConstant,
            &[
                ("scope", Node, Omit),
                ("type", Node, Omit),
                ("name", Node, Required),
                ("initializer", Node, Required),
                ("traits", List, Omit),
            ],
        ),
        "RakuAST::VarDeclaration::Term" => (
            C::VarDeclarationTerm,
            &[
                ("scope", Node, Omit),
                ("type", Node, Omit),
                ("name", Node, Required),
                ("initializer", Node, Omit),
            ],
        ),
        _ => return None,
    })
}

/// Whether `class` has a named constructor here (for `.^can("new")`).
// Cost: O(1).
pub(super) fn is_supported(class: RakuAstClass) -> bool {
    matches!(
        class,
        RakuAstClass::Assignment
            | RakuAstClass::StatementSequence
            | RakuAstClass::Nqp
            | RakuAstClass::Label
            | RakuAstClass::FakeSignature
            | RakuAstClass::RegexNested
            | RakuAstClass::VarNamedCapture
    ) || schema(class.printed_name()).is_some()
}

/// `CLASS.new(:name(...), ...)` for a class listed above, or `None` for any
/// other class.
// Cost: O(a + k), a = arguments, k = items of the list arguments.
pub(super) fn construct(class_name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(built) = positional(class_name, args) {
        return Some(built);
    }
    let (class, schema) = schema(class_name)?;
    Some(build(class_name, class, schema, args))
}

/// The constructors that are not a list of named arguments:
/// `Assignment.new(:item)` (the flag is an adverb of the node),
/// `StatementSequence.new(|@statements)` and `Nqp.new($op, |@args)`, whose
/// arguments are positional fields.
// Cost: O(a), a = arguments.
fn positional(class_name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let node = |class, fields| {
        Ok(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class,
            fields,
        })))
    };
    let positional_fields = |args: &[Value]| -> Vec<RakuAstField> {
        args.iter()
            .map(|value| RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(value.clone()),
            })
            .collect()
    };
    Some(match class_name {
        "RakuAST::Assignment" => {
            let item = super::named_arg(args, "item").is_some_and(|v| v.truthy());
            let fields = if item {
                vec![RakuAstField {
                    name: None,
                    value: RakuAstFieldValue::Adverb("item"),
                }]
            } else {
                Vec::new()
            };
            node(RakuAstClass::Assignment, fields)
        }
        // `FakeSignature.new($signature, :$block)`.
        "RakuAST::FakeSignature" => {
            let positional: Vec<Value> = args
                .iter()
                .filter(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
                .cloned()
                .collect();
            let [signature] = positional.as_slice() else {
                return Some(Err(RuntimeError::new(
                    "RakuAST::FakeSignature.new expects a signature",
                )));
            };
            let mut fields = positional_fields(std::slice::from_ref(signature));
            if let Some(block) = super::named_arg(args, "block") {
                fields.push(RakuAstField {
                    name: Some("block"),
                    value: RakuAstFieldValue::Node(block),
                });
            }
            node(RakuAstClass::FakeSignature, fields)
        }
        // `Regex::Nested.new($goal, $expr)`: two positional regexes.
        "RakuAST::Regex::Nested" => match args {
            [_, _] => node(RakuAstClass::RegexNested, positional_fields(args)),
            _ => Err(RuntimeError::new(
                "RakuAST::Regex::Nested.new expects a goal and an expression",
            )),
        },
        // `Var::NamedCapture.new($index, :$sigil, :@colonpairs)`.
        "RakuAST::Var::NamedCapture" => {
            let positional: Vec<Value> = args
                .iter()
                .filter(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
                .cloned()
                .collect();
            let [index] = positional.as_slice() else {
                return Some(Err(RuntimeError::new(
                    "RakuAST::Var::NamedCapture.new expects an index",
                )));
            };
            let mut fields = positional_fields(std::slice::from_ref(index));
            if let Some(sigil) = super::named_arg(args, "sigil") {
                fields.push(RakuAstField {
                    name: Some("sigil"),
                    value: RakuAstFieldValue::Node(sigil),
                });
            }
            if let Some(pairs) = super::named_arg(args, "colonpairs")
                && let Some(items) = pairs.as_list_items()
                && !items.is_empty()
            {
                fields.push(RakuAstField {
                    name: Some("colonpairs"),
                    value: RakuAstFieldValue::List(items.to_vec()),
                });
            }
            node(RakuAstClass::VarNamedCapture, fields)
        }
        "RakuAST::StatementSequence" => {
            node(RakuAstClass::StatementSequence, positional_fields(args))
        }
        // rakudo's signature is `(str $name)`, and `.raku` prints it that
        // way: a positional field, as the read direction stores it. A
        // `name => ...` spelling fills the same field.
        "RakuAST::Label" => {
            let name = match args {
                [only] if only.as_str().is_some() => only.clone(),
                _ => match super::named_arg(args, "name") {
                    Some(name) => name,
                    None => {
                        return Some(Err(RuntimeError::new(
                            "RakuAST::Label.new expects the label's name",
                        )));
                    }
                },
            };
            node(
                RakuAstClass::Label,
                vec![RakuAstField {
                    name: None,
                    value: RakuAstFieldValue::Node(name),
                }],
            )
        }
        "RakuAST::Nqp" => match args.first() {
            Some(op) if op.as_str().is_some() => node(RakuAstClass::Nqp, positional_fields(args)),
            _ => Err(RuntimeError::new(
                "RakuAST::Nqp.new expects the operation name first",
            )),
        },
        _ => return None,
    })
}

fn build(
    class_name: &str,
    class: RakuAstClass,
    schema: &[Arg],
    args: &[Value],
) -> Result<Value, RuntimeError> {
    if args
        .iter()
        .any(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
    {
        return Err(RuntimeError::new(format!(
            "{class_name}.new takes named arguments only"
        )));
    }
    let mut fields = Vec::with_capacity(schema.len());
    for &(name, kind, missing) in schema {
        let given = super::named_arg(args, name)
            .filter(|v| !v.is_nil())
            // An empty `ArgList` is the default `args`; rakudo's `.raku` leaves
            // it out as it does when the argument is not passed.
            .filter(|v| !(name == "args" && matches!(missing, Omit) && is_empty_arg_list(v)))
            .filter(|v| {
                name != "scope"
                    || !DEFAULT_SCOPES
                        .iter()
                        .any(|(class, scope)| *class == class_name && v.as_str() == Some(scope))
            });
        let value = match (given, missing) {
            (Some(value), _) => value,
            (None, Required) => {
                return Err(RuntimeError::new(format!(
                    "{class_name}.new requires a `{name}` argument"
                )));
            }
            (None, Omit) => continue,
            (None, DefaultStr(s)) => Value::str(s.to_string()),
            (None, DefaultFalse) => Value::truth(false),
        };
        let value = match kind {
            Node => RakuAstFieldValue::Node(value),
            Flag if value.truthy() => RakuAstFieldValue::Node(Value::truth(true)),
            Flag => continue,
            List => {
                let items = value.as_list_items().ok_or_else(|| {
                    RuntimeError::new(format!("{class_name}.new expects `{name}` to be a list"))
                })?;
                if items.is_empty() && matches!(missing, Omit) {
                    continue;
                }
                RakuAstFieldValue::List(
                    items
                        .iter()
                        .map(|item| item.with_deref(|v| v.descalarize().clone()))
                        .collect(),
                )
            }
        };
        fields.push(RakuAstField {
            name: Some(name),
            value,
        });
    }
    Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    })))
}

/// Whether `value` is a `RakuAST::ArgList` with no arguments.
// Cost: O(1).
pub(super) fn is_empty_arg_list(value: &Value) -> bool {
    matches!(value.view(), ValueView::RakuAst(node)
        if node.class == RakuAstClass::ArgList && node.fields.is_empty())
}
