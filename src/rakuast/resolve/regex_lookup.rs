//! `RakuAST::Regex::Assertion::Named(::Args).get-implicit-lookups` (#17501):
//! the `<name>` assertion looks up `&name` lexically, so a `my regex` /
//! `token` / `rule` the unit declares is its resolution, as rakudo's resolver
//! records it. A builtin rule (`<alpha>`, `<ident>`) has an unresolved lookup.

use super::super::view::leaf_field;
use super::super::{RakuAstClass as C, RakuAstFieldValue, RakuAstNode};
use std::collections::HashMap;

use super::{Scopes, opens_scope};
use crate::value::{Value, ValueView};

/// Whether `class` is a regex declaration a `<name>` assertion resolves to.
// Cost: O(1).
pub(super) fn is_regex_declaration(class: C) -> bool {
    matches!(
        class,
        C::RegexDeclaration | C::TokenDeclaration | C::RuleDeclaration
    )
}

/// The identifier a regex declaration or a named assertion is named by.
// Cost: O(1).
pub(super) fn rule_name(node: &RakuAstNode) -> Option<String> {
    let field = node.fields.iter().find(|f| f.name == Some("name"))?;
    let RakuAstFieldValue::Node(name) = &field.value else {
        return None;
    };
    let ValueView::RakuAst(name) = name.view() else {
        return None;
    };
    match super::super::name_parts::name_shape(name) {
        Some(super::super::name_parts::NameShape::Identifier(name)) => Some(name),
        _ => None,
    }
}

/// Record on a named assertion the `Var::Lexical` it looks up, resolved to
/// the declaration in scope when there is one.
// Cost: O(d), d = scope depth.
pub(super) fn record(node: &mut RakuAstNode, scopes: &Scopes) {
    if !matches!(
        node.class,
        C::RegexAssertionNamed | C::RegexAssertionNamedArgs
    ) {
        return;
    }
    let Some(name) = rule_name(node) else {
        return;
    };
    let mut lookup = lookup_node(&name);
    lookup.origin.2 = scopes.find(&format!("&{name}")).cloned();
    node.origin.2 = Some(Value::rakuast(Box::new(lookup)));
}

/// What `get-implicit-lookups` answers on `node`: its recorded lookup, or
/// `None` when `node` is no named assertion.
// Cost: O(1).
pub(in crate::rakuast) fn implicit_lookups(node: &RakuAstNode) -> Option<Value> {
    if !matches!(
        node.class,
        C::RegexAssertionNamed | C::RegexAssertionNamedArgs
    ) {
        return None;
    }
    // A node no resolution pass walked still looks its name up, unresolved.
    let lookup = node
        .origin
        .2
        .clone()
        .or_else(|| rule_name(node).map(|name| Value::rakuast(Box::new(lookup_node(&name)))));
    Some(Value::array(lookup.into_iter().collect()))
}

/// The unresolved `Var::Lexical` `&name` a `<name>` assertion looks up.
// Cost: O(1).
fn lookup_node(name: &str) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: C::VarLexical,
        fields: vec![leaf_field(None, Value::str(format!("&{name}")))],
    }
}

/// Record the lookup of every named assertion under `node` against the regex
/// declarations of the scopes enclosing it, and nothing else: the pass
/// `Str.AST` runs, which must leave the tree's shape as it is.
// Cost: O(n * d), n = nodes under `node`, d = scope depth.
pub(in crate::rakuast) fn annotate_assertions(node: &mut RakuAstNode) {
    let mut scopes = vec![regex_declarations(node)];
    walk_assertions(node, &mut scopes);
}

/// The `&name` of each regex declaration `node`'s own scope makes.
// Cost: O(size of the scope's own statements).
fn regex_declarations(node: &RakuAstNode) -> HashMap<String, Value> {
    let mut found = HashMap::new();
    let visit = |child: &Value, found: &mut HashMap<String, Value>| {
        let ValueView::RakuAst(held) = child.view() else {
            return;
        };
        if is_regex_declaration(held.class)
            && let Some(name) = rule_name(held)
        {
            found.insert(
                format!("&{name}"),
                Value::rakuast(Box::new(RakuAstNode::clone(held))),
            );
        }
        if !opens_scope(held.class) {
            found.extend(regex_declarations(held));
        }
    };
    for field in &node.fields {
        match &field.value {
            RakuAstFieldValue::Node(child) => visit(child, &mut found),
            RakuAstFieldValue::List(items) => items.iter().for_each(|i| visit(i, &mut found)),
            RakuAstFieldValue::Adverb(_) => {}
        }
    }
    found
}

// Cost: O(n * d), n = nodes under `node`, d = scope depth.
fn walk_assertions(node: &mut RakuAstNode, scopes: &mut Vec<HashMap<String, Value>>) {
    let opened = opens_scope(node.class);
    if opened {
        scopes.push(regex_declarations(node));
    }
    for field in &mut node.fields {
        let children: &mut [Value] = match &mut field.value {
            RakuAstFieldValue::Node(child) => std::slice::from_mut(child),
            RakuAstFieldValue::List(items) => items.as_mut_slice(),
            RakuAstFieldValue::Adverb(_) => &mut [],
        };
        for child in children {
            let ValueView::RakuAst(held) = child.view() else {
                continue;
            };
            let mut owned = RakuAstNode::clone(held);
            walk_assertions(&mut owned, scopes);
            *child = Value::rakuast(Box::new(owned));
        }
    }
    if opened {
        scopes.pop();
    }
    if matches!(
        node.class,
        C::RegexAssertionNamed | C::RegexAssertionNamedArgs
    ) && let Some(name) = rule_name(node)
    {
        let lexical = format!("&{name}");
        let mut lookup = lookup_node(&name);
        lookup.origin.2 = scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(&lexical))
            .cloned();
        node.origin.2 = Some(Value::rakuast(Box::new(lookup)));
    }
}

/// The statement list that evaluates the quoted regex `regex` with the
/// `my regex` / `token` / `rule` declarations its `<name>` assertions
/// resolved to declared ahead of it, so `QuotedRegex.compile-time-value`
/// matches `<name>` as the lexical rule (a `.AST` of
/// `my regex pair { a a }; /<?pair> a/`). `None` when it resolved none.
// Cost: O(n), n = nodes of the regex.
pub(crate) fn regex_with_declarations(regex: &Value) -> Option<Value> {
    let mut declarations: Vec<(String, Value)> = Vec::new();
    collect_declarations(regex, &mut declarations);
    if declarations.is_empty() {
        return None;
    }
    let statement = |node: &Value| {
        Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: C::StatementExpression,
            fields: vec![super::super::RakuAstField {
                name: Some("expression"),
                value: RakuAstFieldValue::Node(node.clone()),
            }],
        }))
    };
    let mut statements: Vec<Value> = declarations
        .iter()
        .map(|(_, decl)| statement(decl))
        .collect();
    statements.push(statement(regex));
    Some(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: C::StatementList,
        fields: statements
            .into_iter()
            .map(|s| super::super::RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(s),
            })
            .collect(),
    })))
}

// Cost: O(n), n = nodes under `value`.
fn collect_declarations(value: &Value, out: &mut Vec<(String, Value)>) {
    let ValueView::RakuAst(node) = value.view() else {
        return;
    };
    if matches!(
        node.class,
        C::RegexAssertionNamed | C::RegexAssertionNamedArgs
    ) && let Some(lookup) = &node.origin.2
        && let ValueView::RakuAst(lookup) = lookup.view()
        && let Some(declaration) = &lookup.origin.2
        && let ValueView::RakuAst(decl) = declaration.view()
        && is_regex_declaration(decl.class)
        && let Some(name) = rule_name(decl)
        && !out.iter().any(|(seen, _)| *seen == name)
    {
        out.push((name, declaration.clone()));
    }
    for field in &node.fields {
        match &field.value {
            RakuAstFieldValue::Node(child) => collect_declarations(child, out),
            RakuAstFieldValue::List(items) => {
                items.iter().for_each(|i| collect_declarations(i, out))
            }
            RakuAstFieldValue::Adverb(_) => {}
        }
    }
}
