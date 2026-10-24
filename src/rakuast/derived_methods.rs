//! RakuAST methods whose answer rakudo computes from a node's attributes
//! rather than reading one (`Name.is-anonymous`, `Call::Method.dispatch`,
//! `Sub.default-scope`). Each is written after the rakudo 2026.09 method of
//! the same name (`src/Raku/ast/*.rakumod`); rakudo's own `RakuAST::Deparse`
//! reads them, as can any Raku-level walker.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode, node_accessor};
use crate::value::{Value, ValueView};

/// The names [`call`] answers for `class`, for `.^can` / `.^methods`.
// Cost: O(1).
pub(super) fn names(class: RakuAstClass) -> &'static [&'static str] {
    use RakuAstClass::*;
    if default_scope_rule(class).is_some() {
        return match class {
            Class | Role | Grammar | Module | Package | Native => {
                &["default-scope", "scope", "augmented", "declarator"]
            }
            _ => &["default-scope", "scope"],
        };
    }
    match class {
        Name => &["is-empty", "is-anonymous"],
        CallMethod => &["dispatch", "dispatcher", "macroish"],
        CallQuotedMethod | CallNameAsMethod | CallTermAsMethod | CallPrivateMethod => {
            &["dispatch", "dispatcher"]
        }
        VarDeclarationSimple => &["original-type"],
        Signature => &["parameters-initialized"],
        Sub | Method | Submethod | RegexDeclaration | TokenDeclaration | RuleDeclaration => {
            &["declarator"]
        }
        ApplyInfix | Postfix => &["colonpairs"],
        ApplyPostfix => &["on-topic"],
        Assignment => &["operator"],
        Parameter => &["value"],
        QuotedString => &["has-variables"],
        TypeSubset => &["of"],
        VarDeclarationTerm => &["type"],
        ContextualizerItem | ContextualizerList | ContextualizerHash => &["sigil"],
        _ if statement_prefix_type(class).is_some() => &["type"],
        _ => &[],
    }
}

/// The keyword of a statement prefix or phaser (`.type`): rakudo answers the
/// class's last name segment, lowercased for a prefix (`gather`, `once`) and
/// uppercased for a phaser (`BEGIN`, `PRE`).
// Cost: O(n), n = length of the class name.
fn statement_prefix_type(class: RakuAstClass) -> Option<String> {
    let name = class.printed_name();
    if let Some(phaser) = name.strip_prefix("RakuAST::StatementPrefix::Phaser::") {
        return Some(phaser.to_uppercase());
    }
    let prefix = name.strip_prefix("RakuAST::StatementPrefix::")?;
    (!prefix.contains("::")).then(|| prefix.to_lowercase())
}

/// The special-op method names a `Call::Method` compiles to a primitive op
/// (rakudo's `IMPL-SPECIAL-OP`), which `.macroish` reports.
const SPECIAL_OPS: &[&str] = &["WHAT", "HOW", "WHO", "VAR", "WHERE", "REPR", "DEFINITE"];

/// The answer to `method` on `node` when it is one of the computed methods.
// Cost: O(f), f = fields of the node (a name's parts for the Name methods).
pub(super) fn call(node: &RakuAstNode, method: &str) -> Option<Value> {
    use RakuAstClass::*;
    match (node.class, method) {
        // rakudo answers these through nqp integer comparisons: `0`/`1`, and
        // `True` for an anonymous name (the last part's `is-empty`).
        (Name, "is-empty") => {
            let parts = name_parts(node);
            let empty = parts.is_empty() || (parts.len() == 1 && is_empty_part(&parts[0]));
            Some(Value::int(i64::from(empty)))
        }
        (Name, "is-anonymous") => {
            // The name is just `::`.
            let parts = name_parts(node);
            Some(if parts.len() == 2 && parts.iter().all(is_empty_part) {
                Value::truth(true)
            } else {
                Value::int(0)
            })
        }
        (
            CallMethod | CallQuotedMethod | CallNameAsMethod | CallTermAsMethod | CallPrivateMethod,
            "dispatch" | "dispatcher",
        ) => {
            // mutsu keeps the written dispatcher (`.?`, `.+`, `.*`, `.=`) as a
            // `dispatch` field; rakudo stores `dispatch:<.?>` and derives the
            // short spelling from it. A plain `.` call has neither.
            let short = field(node, "dispatch")
                .and_then(|v| str_of(&v))
                .filter(|s| !s.is_empty() && s != ".")
                .unwrap_or_default();
            Some(Value::str(if method == "dispatch" || short.is_empty() {
                short
            } else {
                format!("dispatch:<{short}>")
            }))
        }
        // mutsu records only the written type, which is what rakudo's
        // `original-type` is before a `where`/coercion rewrite of `type`.
        (VarDeclarationSimple, "original-type") => Some(field(node, "type").unwrap_or(Value::NIL)),
        // A signature whose parameter list was given at all (even empty), or
        // its return type. A parsed signature always has one.
        (Signature, "parameters-initialized") => Some(Value::truth(
            has_field(node, "parameters") || has_field(node, "returns"),
        )),
        (_, "default-scope") => default_scope(node).map(Value::str_from),
        (_, "scope") if default_scope_rule(node.class).is_some() => {
            match field(node, "scope").and_then(|v| str_of(&v)) {
                Some(scope) => Some(Value::str(scope)),
                None => default_scope(node).map(Value::str_from),
            }
        }
        (Class | Role | Grammar | Module | Package | Native, "augmented") => Some(Value::truth(
            field(node, "augmented").is_some_and(|v| v.truthy()),
        )),
        (Sub, "declarator") => Some(Value::str_from("sub")),
        (Class, "declarator") => Some(Value::str_from("class")),
        (Role, "declarator") => Some(Value::str_from("role")),
        (Grammar, "declarator") => Some(Value::str_from("grammar")),
        (Module, "declarator") => Some(Value::str_from("module")),
        (Package, "declarator") => Some(Value::str_from("package")),
        (Native, "declarator") => Some(Value::str_from("native")),
        (Method, "declarator") => Some(Value::str_from("method")),
        (Submethod, "declarator") => Some(Value::str_from("submethod")),
        (RegexDeclaration, "declarator") => Some(Value::str_from("regex")),
        (TokenDeclaration, "declarator") => Some(Value::str_from("token")),
        (RuleDeclaration, "declarator") => Some(Value::str_from("rule")),
        // `nqp::istrue` of the special-op lookup: an integer.
        (CallMethod, "macroish") => {
            let special = field(node, "name")
                .and_then(|name| match name.view() {
                    ValueView::RakuAst(n) => {
                        super::name_parts::name_shape(n).and_then(|shape| match shape {
                            super::name_parts::NameShape::Identifier(s) => Some(s),
                            _ => None,
                        })
                    }
                    _ => None,
                })
                .is_some_and(|s| SPECIAL_OPS.contains(&s.as_str()));
            Some(Value::int(i64::from(special)))
        }
        // mutsu keeps an infix's adverbs out of `visit-children`, as rakudo
        // does; the accessor still answers them.
        (ApplyInfix | Postfix, "colonpairs") => {
            Some(field(node, "colonpairs").unwrap_or_else(|| Value::array(Vec::new())))
        }
        (ApplyPostfix, "on-topic") => {
            Some(Value::truth(field(node, "operand").is_some_and(|op| {
                matches!(op.view(), ValueView::RakuAst(n)
                if n.class == VarLexical
                    && n.fields.first().is_some_and(|f| matches!(&f.value,
                        RakuAstFieldValue::Node(v) if str_of(v).as_deref() == Some("$_"))))
            })))
        }
        (Assignment, "operator") => Some(Value::str_from("=")),
        // A literal-value parameter (`sub f(1)`); any other answers `Mu`.
        (Parameter, "value") => Some(
            field(node, "value")
                .unwrap_or_else(|| Value::package(crate::symbol::Symbol::intern("Mu"))),
        ),
        (QuotedString, "has-variables") => Some(Value::truth(quoted_has_variables(node))),
        (TypeSubset, "of") | (VarDeclarationTerm, "type") => {
            Some(field(node, method).unwrap_or(Value::NIL))
        }
        (ContextualizerItem, "sigil") => Some(Value::str_from("$")),
        (ContextualizerList, "sigil") => Some(Value::str_from("@")),
        (ContextualizerHash, "sigil") => Some(Value::str_from("%")),
        (_, "type") => statement_prefix_type(node.class).map(Value::str),
        _ => None,
    }
}

/// Whether anything in a quoted string is interpolated; a nested quote counts
/// by its own segments (rakudo's `QuotedString.has-variables`).
// Cost: O(s), s = segments, nested ones included.
fn quoted_has_variables(node: &RakuAstNode) -> bool {
    let segments = field(node, "segments")
        .and_then(|v| v.as_list_items().map(<[Value]>::to_vec))
        .unwrap_or_default();
    segments.iter().any(|segment| match segment.view() {
        ValueView::RakuAst(n) => match n.class {
            RakuAstClass::StrLiteral => false,
            RakuAstClass::QuotedString => quoted_has_variables(n),
            RakuAstClass::QuoteWordsAtom => n.fields.first().is_some_and(|f| match &f.value {
                RakuAstFieldValue::Node(atom) => match atom.view() {
                    ValueView::RakuAst(a) => match a.class {
                        RakuAstClass::StrLiteral => false,
                        RakuAstClass::QuotedString => quoted_has_variables(a),
                        _ => true,
                    },
                    _ => false,
                },
                _ => false,
            }),
            _ => true,
        },
        _ => false,
    })
}

/// How a declaration's default scope follows from its class (rakudo's
/// `default-scope` methods).
#[derive(Clone, Copy)]
enum DefaultScope {
    Fixed(&'static str),
    /// `name ?? named !! 'anon'` (a routine).
    ByName(&'static str),
    /// `twigil eq '!' ?? 'has' !! 'my'` (a parameter target).
    ByTwigil,
}

// Cost: O(1).
fn default_scope_rule(class: RakuAstClass) -> Option<DefaultScope> {
    use DefaultScope::*;
    use RakuAstClass::*;
    Some(match class {
        Sub => ByName("my"),
        Method | Submethod | RegexDeclaration | TokenDeclaration | RuleDeclaration => ByName("has"),
        Class
        | Role
        | Grammar
        | Module
        | Package
        | Native
        | TypeEnum
        | TypeSubset
        | VarDeclarationConstant => Fixed("our"),
        VarDeclarationSimple
        | VarDeclarationSignature
        | VarDeclarationTerm
        | VarDeclarationAnonymous
        | TypeCapture
        | ParameterTargetTerm => Fixed("my"),
        ParameterTargetVar => ByTwigil,
        _ => return None,
    })
}

/// The default scope of a routine (Sub, Method, Submethod), which the
/// renderer leaves out when it is the written one.
// Cost: O(f), f = fields of the node.
pub(super) fn routine_default_scope(node: &RakuAstNode) -> Option<&'static str> {
    matches!(
        node.class,
        RakuAstClass::Sub | RakuAstClass::Method | RakuAstClass::Submethod
    )
    .then(|| default_scope(node))
    .flatten()
}

// Cost: O(f), f = fields of the node.
fn default_scope(node: &RakuAstNode) -> Option<&'static str> {
    Some(match default_scope_rule(node.class)? {
        DefaultScope::Fixed(scope) => scope,
        DefaultScope::ByName(named) => {
            if has_field(node, "name") {
                named
            } else {
                "anon"
            }
        }
        DefaultScope::ByTwigil => {
            let name = field(node, "name")
                .and_then(|v| str_of(&v))
                .unwrap_or_default();
            if name.chars().nth(1) == Some('!') {
                "has"
            } else {
                "my"
            }
        }
    })
}

// Cost: O(f), f = fields of the node.
fn field(node: &RakuAstNode, name: &str) -> Option<Value> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .map(|f| match &f.value {
            RakuAstFieldValue::Node(v) => v.clone(),
            RakuAstFieldValue::List(items) => Value::array(items.clone()),
            RakuAstFieldValue::Adverb(_) => Value::truth(true),
        })
}

// Cost: O(f), f = fields of the node.
fn has_field(node: &RakuAstNode, name: &str) -> bool {
    node.fields.iter().any(|f| f.name == Some(name))
}

// Cost: O(1).
fn str_of(value: &Value) -> Option<String> {
    match value.view() {
        ValueView::Str(s) => Some(s.to_string()),
        _ => None,
    }
}

// Cost: O(p), p = parts of the name.
fn name_parts(node: &RakuAstNode) -> Vec<Value> {
    node_accessor(node, "parts")
        .and_then(|v| v.as_list_items().map(<[Value]>::to_vec))
        .unwrap_or_default()
}

// Cost: O(1).
fn is_empty_part(part: &Value) -> bool {
    match part.view() {
        ValueView::RakuAst(n) => {
            matches!(
                n.class,
                RakuAstClass::NamePartEmpty | RakuAstClass::NamePartEmptyEdge
            )
        }
        // A hand-built name may hold the `Name::Part::Empty` type object.
        ValueView::Package(name) => name.with_str(|n| n == "RakuAST::Name::Part::Empty"),
        _ => false,
    }
}
