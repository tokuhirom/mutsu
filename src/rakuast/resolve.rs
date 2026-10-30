//! Name resolution of the unit tree `$*CU` answers (#15610).
//!
//! Rakudo resolves every `RakuAST::Lookup` node of a compilation unit against
//! the declarations in scope before the CHECK phase: `Lookup.is-resolved` says
//! whether it found one and `Lookup.resolution` is the declaration. A tool run
//! from `RakuAST::CompUnit.add-check-code` (`static::CLI`'s analysis) reads
//! them. mutsu's tree comes from the unit text and has none, so this pass adds
//! them once, when the tree for `$*CU` is built.
//!
//! The resolution is kept in the node's metadata ([`super::NodeOrigin`]), not in
//! a field: it never shows in a constructor form, a rendering or a lowering, and
//! takes no part in equality.
//!
//! What resolves (as measured under rakudo):
//!
//! - `Var::Lexical` to the `VarDeclaration::Simple` of an enclosing `my`/`our`/
//!   `state` declaration, or the `ParameterTarget::Var` of a parameter. The
//!   resolution is the very declaration `Value` of the tree, so it has the
//!   declaration's identity (`WHICH`, `nqp::objectid`).
//! - an operator (`Infix`, `Prefix`, `Postfix`, the subscripts) and a call of a
//!   core routine to a `Declaration::External::Setting`; a core type to a
//!   `Declaration::External::Constant`. Their `compile-time-value` is looked up
//!   from the interpreter on demand (see `rakuast_node.rs`).
//!
//! - a unit-declared sub to a stub copy of its `RakuAST::Sub` (without the body,
//!   so the tree stays acyclic); a unit-declared package to a
//!   `Declaration::LexicalPackage`; an imported routine to a
//!   `Declaration::Import`. Subs and packages are visible in their whole scope
//!   (they are hoisted), so a use before the declaration resolves too.
//!
//! Everything else (dynamic variables, unknown names) stays unresolved.

use std::collections::HashMap;

use super::lower::{call_name_str, leaf_str, positional_leaf};
use super::{RakuAstClass as C, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// The declarations visible at the node being walked, innermost scope last:
/// the declaration node `Value` by the name it declares (`$x`, `@a`).
struct Scopes(Vec<HashMap<String, Value>>);

impl Scopes {
    // Cost: O(d), d = scope depth.
    fn find(&self, name: &str) -> Option<&Value> {
        self.0.iter().rev().find_map(|scope| scope.get(name))
    }

    // Cost: O(1) amortized.
    fn declare(&mut self, name: String, declaration: Value) {
        if let Some(scope) = self.0.last_mut() {
            scope.insert(name, declaration);
        }
    }
}

/// Resolve every lookup of the unit tree rooted at `node`.
// Cost: O(n * d), n = nodes of the tree, d = scope depth.
pub(super) fn annotate(node: &mut RakuAstNode) {
    let mut scopes = Scopes(vec![HashMap::new()]);
    walk(node, &mut scopes);
}

/// Whether entering `class` opens a lexical scope.
// Cost: O(1).
fn opens_scope(class: C) -> bool {
    matches!(
        class,
        C::CompUnit
            | C::Block
            | C::PointyBlock
            | C::Sub
            | C::Method
            | C::Submethod
            | C::Class
            | C::Role
            | C::Grammar
            | C::Module
            | C::Package
    )
}

// Cost: O(size of the subtree * d).
fn walk(node: &mut RakuAstNode, scopes: &mut Scopes) {
    let opened = opens_scope(node.class);
    if opened {
        scopes.0.push(HashMap::new());
        hoist(node, scopes);
    }
    let parameter_type = (node.class == C::Parameter)
        .then(|| type_name_of(node))
        .flatten();
    for field in &mut node.fields {
        match &mut field.value {
            RakuAstFieldValue::Node(child) if field.name == Some("target") => {
                if let Some(type_name) = &parameter_type {
                    embed_type(child, type_name);
                }
                visit_child(child, scopes);
            }
            RakuAstFieldValue::Node(child) => visit_child(child, scopes),
            RakuAstFieldValue::List(items) => {
                for item in items {
                    visit_child(item, scopes);
                }
            }
            _ => {}
        }
    }
    if opened {
        scopes.0.pop();
    }
    if let Some(resolution) = resolve(node, scopes) {
        node.origin.2 = Some(resolution);
    }
}

/// Register the subs and packages `node`'s own scope declares, before its
/// statements are walked (they are visible from the start of the scope).
// Cost: O(size of the scope's statements, not counting nested scopes).
fn hoist(node: &RakuAstNode, scopes: &mut Scopes) {
    for field in &node.fields {
        match &field.value {
            RakuAstFieldValue::Node(child) => hoist_child(child, scopes),
            RakuAstFieldValue::List(items) => {
                for item in items {
                    hoist_child(item, scopes);
                }
            }
            _ => {}
        }
    }
}

// Cost: O(size of the subtree, not counting nested scopes).
fn hoist_child(child: &Value, scopes: &mut Scopes) {
    let ValueView::RakuAst(held) = child.view() else {
        return;
    };
    if let Some(name) = super::declared_routines::declared_by(held)
        && held.class == C::Sub
    {
        let mut stub = RakuAstNode::clone(held);
        stub.fields.retain(|f| f.name != Some("body"));
        scopes.declare(format!("&{name}"), Value::rakuast(Box::new(stub)));
    } else if matches!(held.class, C::Class | C::Role | C::Grammar | C::Module | C::Package)
        && let Some(name) = package_name(held)
        && let Some(package) = external("RakuAST::Declaration::LexicalPackage", &name)
    {
        scopes.declare(name, package);
    }
    if !opens_scope(held.class) {
        hoist(held, scopes);
    }
}

/// The identifier a package declaration (`class Bar`) is named by.
// Cost: O(f), f = fields of the node.
fn package_name(node: &RakuAstNode) -> Option<String> {
    let field = node.fields.iter().find(|f| f.name == Some("name"))?;
    let RakuAstFieldValue::Node(name) = &field.value else {
        return None;
    };
    let ValueView::RakuAst(name) = name.view() else {
        return None;
    };
    match super::name_parts::name_shape(name) {
        Some(super::name_parts::NameShape::Identifier(name)) => Some(name),
        _ => None,
    }
}

/// Walk a child node `Value` in place, then register it when it declares a
/// variable (so the declarations after it see it, its own initializer does not).
// Cost: O(size of the subtree * d).
fn visit_child(child: &mut Value, scopes: &mut Scopes) {
    let ValueView::RakuAst(held) = child.view() else {
        return;
    };
    let mut node = RakuAstNode::clone(held);
    walk(&mut node, scopes);
    let class = node.class;
    let name = declared_variable(&node);
    *child = Value::rakuast(Box::new(node));
    if let Some(name) = name
        && matches!(class, C::VarDeclarationSimple | C::ParameterTargetVar)
    {
        scopes.declare(name, child.clone());
    }
}

/// The type a declaration's `type` field names (`Int`), if it has one.
// Cost: O(f), f = fields of the node.
fn type_name_of(node: &RakuAstNode) -> Option<String> {
    let field = node.fields.iter().find(|f| f.name == Some("type"))?;
    let RakuAstFieldValue::Node(ty) = &field.value else {
        return None;
    };
    let ValueView::RakuAst(ty) = ty.view() else {
        return None;
    };
    type_node_name(ty)
}

/// The identifier a `Type::Simple`/`Type::Setting` node names; a `Str:D`
/// (`Type::Definedness`) names its base type.
// Cost: O(f), f = fields of the node.
fn type_node_name(node: &RakuAstNode) -> Option<String> {
    if let Some(base) = node.fields.iter().find(|f| f.name == Some("base-type")) {
        let RakuAstFieldValue::Node(base) = &base.value else {
            return None;
        };
        let ValueView::RakuAst(base) = base.view() else {
            return None;
        };
        return type_node_name(base);
    }
    let field = node
        .fields
        .iter()
        .find(|f| f.name == Some("name") || f.name.is_none())?;
    let RakuAstFieldValue::Node(name) = &field.value else {
        return None;
    };
    let ValueView::RakuAst(name) = name.view() else {
        return None;
    };
    match super::name_parts::name_shape(name) {
        Some(super::name_parts::NameShape::Identifier(name)) => Some(name),
        _ => None,
    }
}

/// Record the declared type of the parameter a `ParameterTarget::Var` belongs
/// to in its metadata (`compile-time-value` answers it).
// Cost: O(1).
fn embed_type(target: &mut Value, type_name: &str) {
    let ValueView::RakuAst(held) = target.view() else {
        return;
    };
    let mut node = RakuAstNode::clone(held);
    node.origin.2 = Some(Value::str(type_name.to_string()));
    *target = Value::rakuast(Box::new(node));
}

/// `compile-time-value` of a variable declaration: the type of its container
/// (`Any` for an untyped scalar, `Array`, `Hash`, `Callable`, `Array[Int]`, the
/// declared type of a typed scalar).
// Cost: O(f), f = fields of the node.
pub(super) fn declared_type(node: &RakuAstNode) -> Value {
    let sigil_name = match node.class {
        C::ParameterTargetVar => leaf(node, "name"),
        _ => leaf(node, "sigil"),
    };
    let sigil = sigil_name.as_deref().and_then(|s| s.chars().next());
    let explicit = match node.class {
        C::ParameterTargetVar => node.origin.2.as_ref().map(Value::to_string_value),
        _ => type_name_of(node),
    }
    .filter(|t| t != "Any");
    let name = match (sigil, explicit) {
        (Some('@'), Some(t)) if node.class == C::VarDeclarationSimple => format!("Array[{t}]"),
        (Some('@'), _) => "Array".to_string(),
        (Some('%'), Some(t)) if node.class == C::VarDeclarationSimple => format!("Hash[{t}]"),
        (Some('%'), _) => "Hash".to_string(),
        (Some('&'), _) => "Callable".to_string(),
        (_, Some(t)) => t,
        _ => "Any".to_string(),
    };
    Value::package(crate::symbol::Symbol::intern(&name))
}

/// A leaf string of `node`: the named field, else its single positional one
/// (`Var::Lexical.new("$x")`, `Infix.new("+")`).
// Cost: O(f), f = fields of the node.
fn leaf(node: &RakuAstNode, name: &str) -> Option<String> {
    leaf_str(node, name)
        .ok()
        .or_else(|| match positional_leaf(node).ok()?.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        })
}

/// The variable (`$x`, `@a`, `&f`) a declaration node declares.
// Cost: O(1).
fn declared_variable(node: &RakuAstNode) -> Option<String> {
    match node.class {
        C::ParameterTargetVar => leaf(node, "name"),
        C::VarDeclarationSimple => {
            let sigil = leaf(node, "sigil")?;
            let field = node.fields.iter().find(|f| f.name == Some("desigilname"))?;
            let RakuAstFieldValue::Node(name) = &field.value else {
                return None;
            };
            let ValueView::RakuAst(name) = name.view() else {
                return None;
            };
            match super::name_parts::name_shape(name) {
                Some(super::name_parts::NameShape::Identifier(name)) => {
                    Some(format!("{sigil}{name}"))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// The resolution of the lookup `node`, when it has one.
// Cost: O(d) for a variable, O(1) otherwise.
fn resolve(node: &RakuAstNode, scopes: &Scopes) -> Option<Value> {
    match node.class {
        C::VarLexical => scopes.find(&leaf(node, "name")?).cloned(),
        C::Infix => external_setting(&format!("infix:<{}>", leaf(node, "operator")?)),
        C::Prefix => external_setting(&format!("prefix:<{}>", leaf(node, "operator")?)),
        C::Postfix => external_setting(&format!("postfix:<{}>", leaf(node, "operator")?)),
        C::PostcircumfixArrayIndex => external_setting("postcircumfix:<[ ]>"),
        C::PostcircumfixHashIndex | C::PostcircumfixLiteralHashIndex => {
            external_setting("postcircumfix:<{ }>")
        }
        C::CallName | C::CallNameWithoutParentheses => {
            let name = call_name_str(node).ok()?;
            if let Some(declared) = scopes.find(&format!("&{name}")) {
                return Some(declared.clone());
            }
            if super::declared_routines::is_declared(&name) {
                return None;
            }
            if super::declared_routines::is_user_call(node) {
                return external("RakuAST::Declaration::Import", &format!("&{name}"));
            }
            if !crate::runtime::Interpreter::is_builtin_function(&name) {
                return None;
            }
            external_setting(&name)
        }
        C::TypeSimple | C::TypeSetting => {
            let name = type_node_name(node)?;
            if let Some(package) = scopes.find(&name) {
                return Some(package.clone());
            }
            if !crate::core_type_names::contains(&name) {
                return None;
            }
            external("RakuAST::Declaration::External::Constant", &name)
        }
        _ => None,
    }
}

/// A core routine's `Declaration::External::Setting`, named by its lexical name
/// without the sigil (`infix:<+>`, `say`).
// Cost: O(1).
fn external_setting(name: &str) -> Option<Value> {
    external(
        "RakuAST::Declaration::External::Setting",
        &format!("&{name}"),
    )
}

/// An external declaration of the given class; its `compile-time-value` is
/// read lazily from the interpreter.
// Cost: O(1).
fn external(class: &str, lexical_name: &str) -> Option<Value> {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(
        "lexical-name".to_string(),
        Value::str(lexical_name.to_string()),
    );
    Some(Value::make_instance(
        crate::symbol::Symbol::intern(class),
        attrs,
    ))
}
