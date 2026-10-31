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
use super::{RakuAstClass as C, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// The declarations visible at the node being walked, innermost scope last:
/// the declaration node `Value` by the name it declares (`$x`, `@a`).
struct Scopes<'a> {
    scopes: Vec<HashMap<String, Value>>,
    /// Whether the interpreter knows a routine of this name (an import).
    imported: &'a dyn Fn(&str) -> bool,
    /// The named packages enclosing the node being walked, innermost last.
    packages: Vec<String>,
    /// How many `*` arguments the enclosing thunk has numbered so far.
    whatever_args: Vec<usize>,
    /// Anonymous subsets minted so far, for their internal names.
    anonymous_subsets: usize,
    /// `multi` declarations met so far per routine name: the next one is that
    /// candidate of the name's dispatcher.
    multi_candidates: HashMap<String, usize>,
}

impl Scopes<'_> {
    // Cost: O(d), d = scope depth.
    fn find(&self, name: &str) -> Option<&Value> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    // Cost: O(1) amortized.
    fn declare(&mut self, name: String, declaration: Value) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, declaration);
        }
    }
}

/// Resolve every lookup of the unit tree rooted at `node`.
// Cost: O(n * d), n = nodes of the tree, d = scope depth.
pub(super) fn annotate(node: &mut RakuAstNode, imported: &dyn Fn(&str) -> bool) {
    let mut scopes = Scopes {
        scopes: vec![HashMap::new()],
        imported,
        packages: Vec::new(),
        whatever_args: vec![0],
        anonymous_subsets: 0,
        multi_candidates: HashMap::new(),
    };
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
    // A role's body holds its name and signature (the role's own children
    // are its name and that body), as rakudo's tree has them.
    if node.class == C::Role {
        move_role_header_into_body(node);
    }
    let opened = opens_scope(node.class);
    let node_class = node.class;
    if opened {
        scopes.scopes.push(HashMap::new());
        hoist(node, scopes);
    }
    // A method remembers the package that declares it, for `compile-time-value`.
    let package = matches!(
        node.class,
        C::Class | C::Role | C::Grammar | C::Module | C::Package
    )
    .then(|| package_name(node))
    .flatten();
    if matches!(node.class, C::Method | C::Submethod) && !scopes.packages.is_empty() {
        node.origin.2 = Some(Value::str(scopes.packages.join("::")));
    }
    // A `multi sub` remembers which candidate of its name it declares.
    if node.class == C::Sub
        && super::lower::leaf_str(node, "multiness").is_ok_and(|m| m == "multi")
        && let Some(name) = super::declared_routines::declared_by(node)
    {
        let seen = scopes.multi_candidates.entry(name).or_insert(0);
        node.origin.2 = Some(Value::str(format!("multi:{seen}")));
        *seen += 1;
    }
    let named_package = package.is_some();
    if let Some(name) = package {
        scopes.packages.push(name);
        // A package remembers its qualified name, for `compile-time-value`.
        node.origin.2 = Some(Value::str(scopes.packages.join("::")));
    }
    // `*` arguments are numbered within one thunk: a statement, a parameter's
    // `where`, or a side of a short-circuit operator.
    let numbers_whatever = matches!(node.class, C::StatementExpression | C::Parameter)
        || (node.class == C::ApplyInfix && short_circuits(node));
    if numbers_whatever {
        scopes.whatever_args.push(0);
    }
    if node.class == C::WhateverCodeArgument
        && let Some(count) = scopes.whatever_args.last_mut()
    {
        *count += 1;
        node.origin.2 = Some(Value::str(format!("_whatever_arg_{count}")));
    }
    // `my Int $x where P` is a variable of an anonymous subset, as rakudo's tree
    // has it: the declaration's `type` is a `Type::Subset` (`P` over `Int`).
    if node.class == C::VarDeclarationSimple {
        refine_with_anonymous_subset(node, scopes);
    }
    let parameter_type = (node.class == C::Parameter).then(|| type_field(node));
    for field in &mut node.fields {
        match &mut field.value {
            RakuAstFieldValue::Node(child) if field.name == Some("target") => {
                if let Some(type_node) = &parameter_type {
                    embed_declaration(child, type_node.as_ref());
                }
                visit_child(child, scopes);
            }
            RakuAstFieldValue::Node(child) => {
                visit_child(child, scopes);
                // A block that is a statement's expression is a block statement.
                if node_class == C::StatementExpression
                    && field.name == Some("expression")
                    && let ValueView::RakuAst(held) = child.view()
                    && matches!(held.class, C::Block | C::PointyBlock)
                {
                    let mut marked = RakuAstNode::clone(held);
                    marked.origin.2 = Some(Value::TRUE);
                    *child = Value::rakuast(Box::new(marked));
                }
                // The body of a conditional or a loop runs in place.
                if matches!(
                    node_class,
                    C::StatementIf
                        | C::StatementUnless
                        | C::StatementElsif
                        | C::StatementWith
                        | C::StatementWithout
                        | C::StatementOrwith
                        | C::StatementLoop
                        | C::StatementLoopWhile
                        | C::StatementLoopUntil
                        | C::StatementLoopRepeatWhile
                        | C::StatementLoopRepeatUntil
                ) && let ValueView::RakuAst(held) = child.view()
                    && held.class == C::Block
                {
                    let mut marked = RakuAstNode::clone(held);
                    marked.origin.2 = Some(Value::str("immediate".to_string()));
                    *child = Value::rakuast(Box::new(marked));
                }
            }
            RakuAstFieldValue::List(items) => {
                for item in items {
                    visit_child(item, scopes);
                }
            }
            _ => {}
        }
    }
    // A routine's parameters remember which routine they belong to, so
    // `Parameter.meta-object` can answer the routine's own `Parameter`.
    if let Some(owner) = parameter_owner(node, scopes) {
        annotate_parameters(node, &owner);
    }
    if numbers_whatever {
        scopes.whatever_args.pop();
    }
    if named_package {
        scopes.packages.pop();
    }
    if opened {
        scopes.scopes.pop();
    }
    // `$!name` answers its attribute's declared type as `return-type`.
    if node.class == C::VarAttribute
        && let Some(name) = leaf(node, "name")
        && let Some(decl) = scopes.find(&format!("attr:{name}"))
        && let ValueView::RakuAst(decl) = decl.view()
    {
        node.origin.2 = Some(return_type(decl));
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
    // A `has` attribute is visible to `$!name` in its class's methods.
    if held.class == C::VarDeclarationSimple
        && leaf(held, "scope").as_deref() == Some("has")
        && let Some(variable) = declared_variable(held)
        && let Some(sigil) = variable.chars().next()
    {
        scopes.declare(
            format!("attr:{sigil}!{}", &variable[sigil.len_utf8()..]),
            child.clone(),
        );
    }
    if let Some(name) = super::declared_routines::declared_by(held)
        && held.class == C::Sub
    {
        let mut stub = RakuAstNode::clone(held);
        stub.fields.retain(|f| f.name != Some("body"));
        scopes.declare(format!("&{name}"), Value::rakuast(Box::new(stub)));
    } else if matches!(
        held.class,
        C::Class | C::Role | C::Grammar | C::Module | C::Package
    ) && let Some(name) = package_name(held)
    {
        // The package answers by its full name (`Example::C` in a
        // `unit module Example`), the scope by the name it is written with.
        let full = if scopes.packages.is_empty() {
            name.clone()
        } else {
            format!("{}::{name}", scopes.packages.join("::"))
        };
        if let Some(package) = external("RakuAST::Declaration::LexicalPackage", &full) {
            scopes.declare(name, package);
        }
    }
    if !opens_scope(held.class) {
        hoist(held, scopes);
    }
}

/// Replace the `where` of a `VarDeclaration::Simple` and its `type` by an
/// anonymous `Type::Subset` carrying both (internal name in its origin member).
// Cost: O(f), f = fields of the node.
fn refine_with_anonymous_subset(node: &mut RakuAstNode, scopes: &mut Scopes) {
    let Some(at) = node.fields.iter().position(|f| f.name == Some("where")) else {
        return;
    };
    let where_field = node.fields.remove(at);
    let mut subset_fields = vec![RakuAstField {
        name: Some("where"),
        value: where_field.value,
    }];
    if let Some(at) = node.fields.iter().position(|f| f.name == Some("type")) {
        let base = node.fields.remove(at);
        subset_fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(RakuAstNode {
                origin: Default::default(),
                class: C::TraitOf,
                fields: vec![RakuAstField {
                    name: None,
                    value: base.value,
                }],
            }))]),
        });
    }
    scopes.anonymous_subsets += 1;
    let mut subset = RakuAstNode {
        origin: Default::default(),
        class: C::TypeSubset,
        fields: subset_fields,
    };
    subset.origin.2 = Some(Value::str(format!(
        "__mutsu_anon_subset_ast_{}",
        scopes.anonymous_subsets
    )));
    // `type` is the first field a declaration lists after its scope.
    let insert_at = node
        .fields
        .iter()
        .position(|f| f.name == Some("desigilname"))
        .unwrap_or(0);
    node.fields.insert(
        insert_at,
        RakuAstField {
            name: Some("type"),
            value: RakuAstFieldValue::Node(Value::rakuast(Box::new(subset))),
        },
    );
}

/// Whether the infix of an `ApplyInfix` is a short-circuit operator, whose
/// sides are thunks of their own.
// Cost: O(f), f = fields of the node.
fn short_circuits(node: &RakuAstNode) -> bool {
    node.fields.iter().any(|f| {
        let RakuAstFieldValue::Node(infix) = &f.value else {
            return false;
        };
        let ValueView::RakuAst(infix) = infix.view() else {
            return false;
        };
        infix.class == C::Infix
            && matches!(
                leaf(infix, "operator").as_deref(),
                Some("&&" | "||" | "//" | "and" | "or" | "andthen" | "orelse")
            )
    })
}

/// Who owns the parameters of `node`: `sub\u{1}NAME` or `method\u{1}OWNER\u{1}NAME`.
// Cost: O(f), f = fields of the node.
fn parameter_owner(node: &RakuAstNode, scopes: &Scopes) -> Option<String> {
    match node.class {
        C::Sub => {
            super::declared_routines::declared_by(node).map(|name| {
                match multi_candidate_index(node) {
                    Some(candidate) => format!("sub\u{1}{name}\u{1}multi:{candidate}"),
                    None => format!("sub\u{1}{name}"),
                }
            })
        }
        C::Method | C::Submethod if !scopes.packages.is_empty() => {
            let name = package_name(node)?;
            Some(format!(
                "method\u{1}{}\u{1}{name}",
                scopes.packages.join("::")
            ))
        }
        _ => None,
    }
}

/// Stamp every parameter of the routine node's signature with its owner and
/// position (`owner\u{1}INDEX`).
// Cost: O(p), p = parameters of the routine.
fn annotate_parameters(node: &mut RakuAstNode, owner: &str) {
    for field in &mut node.fields {
        let RakuAstFieldValue::Node(signature) = &mut field.value else {
            continue;
        };
        let ValueView::RakuAst(held) = signature.view() else {
            continue;
        };
        if held.class != C::Signature {
            continue;
        }
        let mut sig = RakuAstNode::clone(held);
        for sig_field in &mut sig.fields {
            let RakuAstFieldValue::List(items) = &mut sig_field.value else {
                continue;
            };
            for (index, item) in items.iter_mut().enumerate() {
                let ValueView::RakuAst(param) = item.view() else {
                    continue;
                };
                if param.class != C::Parameter {
                    continue;
                }
                let mut marked = RakuAstNode::clone(param);
                marked.origin.2 = Some(Value::str(format!("{owner}\u{1}{index}")));
                *item = Value::rakuast(Box::new(marked));
            }
        }
        *signature = Value::rakuast(Box::new(sig));
    }
}

/// The routine and position a `Parameter` of the unit tree belongs to.
// Cost: O(1).
pub(super) fn parameter_owner_of(node: &RakuAstNode) -> Option<Vec<String>> {
    let tag = node.origin.2.as_ref()?.to_string_value();
    Some(tag.split('\u{1}').map(str::to_string).collect())
}

/// The package and name of a method declared in a named package of the unit
/// tree `$*CU` answers: what `Method.compile-time-value` looks up.
// Cost: O(f), f = fields of the node.
pub(super) fn declared_method(node: &RakuAstNode) -> Option<(String, String)> {
    let owner = node.origin.2.as_ref()?.to_string_value();
    Some((owner, package_name(node)?))
}

/// The identifier a package declaration (`class Bar`) is named by.
// Cost: O(f), f = fields of the node.
pub(super) fn package_name(node: &RakuAstNode) -> Option<String> {
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
pub(super) fn type_node_name(node: &RakuAstNode) -> Option<String> {
    // An anonymous subset made by [`refine_with_anonymous_subset`].
    if node.class == C::TypeSubset
        && let Some(name) = node.origin.2.as_ref()
    {
        return Some(name.to_string_value());
    }
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

/// [`type_node_name`] keeping a parameterization: `Array[Int]` for
/// `Type::Parameterized`, its arguments named the same way.
// Cost: O(size of the type node).
fn type_node_full_name(node: &RakuAstNode) -> Option<String> {
    if node.class != C::TypeParameterized {
        return type_node_name(node);
    }
    let base = type_node_full_name(held_node(node, "base-type")?)?;
    let args = held_node(node, "args")?;
    let mut names = Vec::new();
    for field in &args.fields {
        match &field.value {
            RakuAstFieldValue::List(items) => {
                for item in items {
                    let ValueView::RakuAst(arg) = item.view() else {
                        return None;
                    };
                    names.push(type_node_full_name(arg)?);
                }
            }
            RakuAstFieldValue::Node(item) => {
                let ValueView::RakuAst(arg) = item.view() else {
                    return None;
                };
                names.push(type_node_full_name(arg)?);
            }
            _ => {}
        }
    }
    Some(format!("{base}[{}]", names.join(",")))
}

/// The node held by the named field of `node`.
// Cost: O(f), f = fields of the node.
fn held_node<'a>(node: &'a RakuAstNode, field: &str) -> Option<&'a RakuAstNode> {
    match &node.fields.iter().find(|f| f.name == Some(field))?.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::RakuAst(n) => Some(n),
            _ => None,
        },
        _ => None,
    }
}

/// The `type` field of a declaration node, if it has one.
// Cost: O(f), f = fields of the node.
fn type_field(node: &RakuAstNode) -> Option<Value> {
    match &node.fields.iter().find(|f| f.name == Some("type"))?.value {
        RakuAstFieldValue::Node(ty) => Some(ty.clone()),
        _ => None,
    }
}

/// Record the `VarDeclaration::Simple` a `ParameterTarget::Var` declares in its
/// metadata: `ParameterTarget::Var.declaration` answers it, and
/// `compile-time-value` reads the declared type off it. It is built once, so it
/// keeps its identity.
// Cost: O(1).
fn embed_declaration(target: &mut Value, type_node: Option<&Value>) {
    let ValueView::RakuAst(held) = target.view() else {
        return;
    };
    let mut node = RakuAstNode::clone(held);
    let Some(name) = leaf(&node, "name") else {
        return;
    };
    let mut chars = name.chars();
    let (Some(sigil), rest) = (chars.next(), chars.as_str()) else {
        return;
    };
    let mut fields = vec![
        super::convert::leaf_field(Some("sigil"), Value::str(sigil.to_string())),
        super::convert::leaf_field(
            Some("desigilname"),
            Value::rakuast(Box::new(super::name_parts::name_from_parts(vec![
                super::name_parts::simple_part(rest),
            ]))),
        ),
    ];
    // The constructor's own order (`type` first), which the field lookups of a
    // declaration read.
    if let Some(ty) = type_node {
        fields.insert(0, super::convert::leaf_field(Some("type"), ty.clone()));
    }
    node.origin.2 = Some(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: C::VarDeclarationSimple,
        fields,
    })));
    *target = Value::rakuast(Box::new(node));
}

/// The `VarDeclaration::Simple` of a `ParameterTarget::Var` ([`embed_declaration`]).
// Cost: O(1).
pub(super) fn target_declaration(node: &RakuAstNode) -> Option<Value> {
    (node.class == C::ParameterTargetVar)
        .then(|| node.origin.2.clone())
        .flatten()
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
        C::ParameterTargetVar => target_declaration(node).and_then(|d| match d.view() {
            ValueView::RakuAst(d) => type_name_of(d),
            _ => None,
        }),
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

/// `VarDeclaration::Simple.return-type`: the type of the declared container as
/// rakudo answers it: `Mu` when untyped, the declared type (`Int`, `Str:D`),
/// `Positional`/`Associative` (parameterized by a declared element type) and
/// `Callable` for the sigiled forms.
// Cost: O(f), f = fields of the node.
pub(super) fn return_type(node: &RakuAstNode) -> Value {
    let sigil = leaf(node, "sigil").and_then(|s| s.chars().next());
    let element = declared_type_with_smiley(node);
    let name = match (sigil, element) {
        (Some('@'), Some(t)) => format!("Positional[{t}]"),
        (Some('@'), None) => "Positional".to_string(),
        (Some('%'), element) => match hash_parameters(node, element) {
            Some(parameters) => format!("Associative[{parameters}]"),
            None => "Associative".to_string(),
        },
        (Some('&'), _) => "Callable".to_string(),
        (_, Some(t)) => t,
        _ => "Mu".to_string(),
    };
    Value::package(crate::symbol::Symbol::intern(&name))
}

/// The key type a hash declaration's shape names: `Int` for `my Int %h{Int}`.
// Cost: O(size of the shape).
fn shape_key_name(node: &RakuAstNode) -> Option<String> {
    let shape = held_node(node, "shape")?;
    let statement = shape.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::RakuAst(n) => Some(n),
            _ => None,
        },
        _ => None,
    })?;
    let expression = held_node(statement, "expression")?;
    type_node_full_name(expression)
}

/// The parameters of a hash declaration's `Associative[..]` / `Hash[..]`:
/// the element type, then the key type of a shape (`Any` stands in for an
/// undeclared element type: `my %h{Int}` is `Hash[Any,Int]`).
// Cost: O(size of the declaration's type and shape).
fn hash_parameters(node: &RakuAstNode, element: Option<String>) -> Option<String> {
    match (element, shape_key_name(node)) {
        (Some(element), Some(key)) => Some(format!("{element},{key}")),
        (None, Some(key)) => Some(format!("Any,{key}")),
        (Some(element), None) => Some(element),
        (None, None) => None,
    }
}

/// The declared type of `node` including a definiteness smiley (`Str:D`).
// Cost: O(f), f = fields of the node.
fn declared_type_with_smiley(node: &RakuAstNode) -> Option<String> {
    let field = node.fields.iter().find(|f| f.name == Some("type"))?;
    let RakuAstFieldValue::Node(ty) = &field.value else {
        return None;
    };
    let ValueView::RakuAst(ty) = ty.view() else {
        return None;
    };
    let base = type_node_full_name(ty)?;
    let smiley = ty
        .fields
        .iter()
        .find(|f| f.name == Some("definite"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => Some(if v.truthy() { ":D" } else { ":U" }),
            _ => None,
        })
        .unwrap_or("");
    Some(format!("{base}{smiley}"))
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

/// `VarDeclaration::Simple.container-type`: the container class of the
/// sigil (`Array` for `@`, `Hash` for `%`, `Scalar` otherwise).
// Cost: O(f), f = fields of the node.
pub(super) fn container_type(node: &RakuAstNode) -> Value {
    let element = declared_type_with_smiley(node);
    let name = match (leaf(node, "sigil").as_deref(), element) {
        (Some("@"), Some(t)) => format!("Array[{t}]"),
        (Some("@"), None) => "Array".to_string(),
        (Some("%"), element) => match hash_parameters(node, element) {
            Some(parameters) => format!("Hash[{parameters}]"),
            None => "Hash".to_string(),
        },
        _ => "Scalar".to_string(),
    };
    Value::package(crate::symbol::Symbol::intern(&name))
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
                return (scopes.imported)(&name)
                    .then(|| external("RakuAST::Declaration::Import", &format!("&{name}")))
                    .flatten();
            }
            external_setting(&name)
        }
        // `True`, `Inf`, ... : a core enum value, constant of the setting.
        C::TermEnum => external(
            "RakuAST::Declaration::External::Constant",
            &type_node_name(node).or_else(|| leaf(node, "name"))?,
        ),
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

/// Which candidate of its name's dispatcher a `multi sub` node declares.
// Cost: O(1).
pub(super) fn multi_candidate_index(node: &RakuAstNode) -> Option<usize> {
    node.origin
        .2
        .as_ref()?
        .to_string_value()
        .strip_prefix("multi:")?
        .parse()
        .ok()
}

/// The qualified name of a package declaration of the unit tree `$*CU` answers.
// Cost: O(1).
pub(super) fn package_qualified_name(node: &RakuAstNode) -> Option<String> {
    matches!(
        node.class,
        C::Class | C::Role | C::Grammar | C::Module | C::Package
    )
    .then(|| node.origin.2.as_ref().map(|n| n.to_string_value()))
    .flatten()
}

/// Move a `Role`'s `parameterization` signature, behind its name, into its
/// `RoleBody` (the hidden `name` and `signature` fields rakudo's body holds).
// Cost: O(f), f = fields of the role and its body.
fn move_role_header_into_body(role: &mut RakuAstNode) {
    let name = role
        .fields
        .iter()
        .find(|f| f.name == Some("name"))
        .map(|f| f.value.clone());
    let signature = role
        .fields
        .iter()
        .position(|f| f.name == Some("parameterization"))
        .map(|at| role.fields.remove(at).value);
    let Some(body_field) = role.fields.iter_mut().find(|f| f.name == Some("body")) else {
        return;
    };
    let RakuAstFieldValue::Node(body) = &body_field.value else {
        return;
    };
    let ValueView::RakuAst(held) = body.view() else {
        return;
    };
    if held.class != C::RoleBody {
        return;
    }
    let mut body_node = RakuAstNode::clone(held);
    let mut header = Vec::new();
    if let Some(value) = name {
        header.push(RakuAstField {
            name: Some("name"),
            value,
        });
    }
    // A role without parameters still has an (empty) signature.
    let value = signature.unwrap_or_else(|| {
        RakuAstFieldValue::Node(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: C::Signature,
            fields: vec![RakuAstField {
                name: Some("parameters"),
                value: RakuAstFieldValue::List(Vec::new()),
            }],
        })))
    });
    header.push(RakuAstField {
        name: Some("signature"),
        value,
    });
    add_implicit_role_statements(&mut body_node);
    header.append(&mut body_node.fields);
    body_node.fields = header;
    body_field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(body_node)));
}

/// Bracket a role body's statements with the ones rakudo's tree adds:
/// `Role::ResolveInstantiations` first and `nqp::list(MetaObject, nqp::curlexpad())`
/// last.
// Cost: O(s), s = statements of the body.
fn add_implicit_role_statements(body: &mut RakuAstNode) {
    let Some(blockoid_field) = body.fields.iter_mut().find(|f| f.name == Some("body")) else {
        return;
    };
    let RakuAstFieldValue::Node(blockoid) = &blockoid_field.value else {
        return;
    };
    let ValueView::RakuAst(blockoid) = blockoid.view() else {
        return;
    };
    let mut blockoid = RakuAstNode::clone(blockoid);
    let Some(list_field) = blockoid.fields.iter_mut().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) => {
            matches!(v.view(), ValueView::RakuAst(n) if n.class == C::StatementList).then_some(f)
        }
        _ => None,
    }) else {
        return;
    };
    let RakuAstFieldValue::Node(list) = &list_field.value else {
        return;
    };
    let ValueView::RakuAst(list) = list.view() else {
        return;
    };
    let mut list = RakuAstNode::clone(list);
    let plain = |class, fields| {
        Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class,
            fields,
        }))
    };
    let unnamed = |value: Value| RakuAstField {
        name: None,
        value: RakuAstFieldValue::Node(value),
    };
    let nqp = |op: &str, args: Vec<Value>| {
        let mut fields = vec![unnamed(Value::str(op.to_string()))];
        fields.extend(args.into_iter().map(unnamed));
        plain(C::Nqp, fields)
    };
    let closing = nqp(
        "list",
        vec![
            plain(C::RoleMetaObject, Vec::new()),
            nqp("curlexpad", Vec::new()),
        ],
    );
    let closing = plain(
        C::StatementExpression,
        vec![RakuAstField {
            name: Some("expression"),
            value: RakuAstFieldValue::Node(closing),
        }],
    );
    list.fields
        .insert(0, unnamed(plain(C::RoleResolveInstantiations, Vec::new())));
    list.fields.push(unnamed(closing));
    list_field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(list)));
    blockoid_field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(blockoid)));
}
