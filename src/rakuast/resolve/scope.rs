//! The scope stack of a `RakuAST::Resolver` (#17501): what a resolver that
//! pushed the unit's scope nodes (`push-scope`) resolves a lexical name to, and
//! the resolution `IMPL-BEGIN` / `IMPL-CHECK` give a node built at CHECK time.

use std::collections::HashMap;

use super::super::{RakuAstClass as C, RakuAstFieldValue, RakuAstNode};
use super::{Scopes, declared_variable, external, external_setting, hoist, opens_scope, walk};
use crate::value::{Value, ValueView};

/// The declarations a scope node makes in its own scope (not in the scopes
/// nested in it): its subs and packages, and its variables and parameters,
/// each by the name it declares (`$x`, `&f`, `Pkg`). What a resolver that
/// pushed `scope` finds there (`RakuAST::Resolver.resolve-lexical`, #17501).
// Cost: O(size of the scope's own statements, not counting nested scopes).
fn scope_declarations(scope: &RakuAstNode) -> HashMap<String, Value> {
    let noop = |_: &str| false;
    let mut scopes = Scopes {
        scopes: vec![HashMap::new()],
        imported: &noop,
        imported_type: &noop,
        shadowed: &noop,
        packages: Vec::new(),
        whatever_args: vec![0],
        anonymous_subsets: 0,
        multi_candidates: HashMap::new(),
    };
    hoist(scope, &mut scopes);
    declare_variables(scope, &mut scopes);
    scopes.scopes.pop().unwrap_or_default()
}

/// Register the variables and parameters `node` declares, and the routines its
/// `use` statements import, not descending into the scopes nested in it.
// Cost: O(size of the subtree, not counting nested scopes, plus the imports).
fn declare_variables(node: &RakuAstNode, scopes: &mut Scopes) {
    let mut visit = |child: &Value| {
        let ValueView::RakuAst(held) = child.view() else {
            return;
        };
        if matches!(held.class, C::VarDeclarationSimple | C::ParameterTargetVar)
            && let Some(name) = declared_variable(held)
        {
            scopes.declare(name, child.clone());
        }
        // A `use` binds what it imports in the pad of the scope it is in:
        // rakudo's resolver finds `&name` there as a `Declaration::Import`.
        if held.class == C::StatementUse {
            for routine in super::super::use_stmt::recorded_imports(held) {
                let name = format!("&{routine}");
                if let Some(import) = external("RakuAST::Declaration::Import", &name) {
                    scopes.declare(name, import);
                }
            }
        }
        if !opens_scope(held.class) {
            declare_variables(held, scopes);
        }
    };
    for field in &node.fields {
        match &field.value {
            RakuAstFieldValue::Node(child) => visit(child),
            RakuAstFieldValue::List(items) => items.iter().for_each(&mut visit),
            _ => {}
        }
    }
}

/// Resolve every lookup of `node`, a tree built at CHECK time, against the
/// declarations of the scopes a resolver pushed (`outer`, outermost first):
/// what `IMPL-BEGIN` / `IMPL-CHECK` do for a node not parsed with its unit.
// Cost: as [`annotate`], plus the declarations of `outer`.
fn annotate_in(
    node: &mut RakuAstNode,
    outer: Vec<HashMap<String, Value>>,
    imported: &dyn Fn(&str) -> bool,
    imported_type: &dyn Fn(&str) -> bool,
) {
    let mut scopes = Scopes {
        scopes: outer,
        imported,
        imported_type,
        shadowed: &|_| false,
        packages: Vec::new(),
        whatever_args: vec![0],
        anonymous_subsets: 0,
        multi_candidates: HashMap::new(),
    };
    scopes.scopes.push(HashMap::new());
    walk(node, &mut scopes);
}

/// The resolution of a lexical `name` (with its sigil) no pushed scope
/// declares: an imported routine, a routine or type of the setting, or a
/// block's implicit `$_` / `$/` / `$!`.
// Cost: O(1) plus the import probe.
fn resolve_outside_scopes(
    name: &str,
    imported: &dyn Fn(&str) -> bool,
    imported_type: &dyn Fn(&str) -> bool,
) -> Option<Value> {
    if matches!(name, "$_" | "$/" | "$!") {
        return external("RakuAST::VarDeclaration::Implicit::Special", name);
    }
    if let Some(routine) = name.strip_prefix('&') {
        if crate::builtin_functions::is_builtin_function(routine) {
            return external_setting(routine);
        }
        return imported(routine)
            .then(|| external("RakuAST::Declaration::Import", name))
            .flatten();
    }
    if name.starts_with(|c: char| c.is_alphabetic() || c == '_') {
        if crate::core_type_names::contains(name) {
            return external("RakuAST::Declaration::External::Constant", name);
        }
        return imported_type(name)
            .then(|| external("RakuAST::Declaration::Import", name))
            .flatten();
    }
    None
}

/// `$resolver.resolve-lexical($name)` with `scopes` pushed (outermost first):
/// the declaration of `name` in the innermost scope that declares it, else an
/// import or the setting's. `None` when nothing declares it.
// Cost: O(s * k), s = pushed scopes, k = size of a scope's own statements.
pub(crate) fn resolve_lexical(
    scopes: &[Value],
    name: &str,
    imported: &dyn Fn(&str) -> bool,
    imported_type: &dyn Fn(&str) -> bool,
) -> Option<Value> {
    for scope in scopes.iter().rev() {
        if let ValueView::RakuAst(node) = scope.view()
            && let Some(found) = scope_declarations(node).remove(name)
        {
            return Some(found);
        }
    }
    resolve_outside_scopes(name, imported, imported_type)
}

/// `$node.IMPL-BEGIN($resolver, $context)` / `IMPL-CHECK`: resolve the
/// lookups of `node` against the declarations of `scopes`, in place.
// Cost: O(n * d) plus the declarations of `scopes`, n = nodes of `node`.
pub(crate) fn resolve_node_in(
    node: &Value,
    scopes: &[Value],
    imported: &dyn Fn(&str) -> bool,
    imported_type: &dyn Fn(&str) -> bool,
) {
    let ValueView::RakuAst(held) = node.view() else {
        return;
    };
    let outer = scopes
        .iter()
        .filter_map(|scope| match scope.view() {
            ValueView::RakuAst(scope) => Some(scope_declarations(scope)),
            _ => None,
        })
        .collect::<Vec<HashMap<String, Value>>>();
    let mut annotated = RakuAstNode::clone(held);
    annotate_in(&mut annotated, outer, imported, imported_type);
    node.replace_rakuast_node(annotated);
}
