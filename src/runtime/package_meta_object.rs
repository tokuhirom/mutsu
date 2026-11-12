//! `meta-object` of a RakuAST package node, declared ahead of `EVAL`
//! (ADR-17472).

use super::*;
use crate::rakuast::declared_ahead;
use crate::rakuast::{RakuAstField, RakuAstFieldValue, RakuAstNode};

impl Interpreter {
    /// `meta-object` of a `RakuAST::Class` / `Role` / `Grammar` / `Package` /
    /// `Module`: the type the node declares. A node whose unit has not run
    /// yet declares it now, from its unit's `use`-like prelude and its own
    /// statement, and the later `EVAL` of the unit adopts that type
    /// (ADR-17472).
    // Cost: O(1) once declared; the first call runs the declaration (the
    // prelude's `use`s and the package body).
    pub(crate) fn package_node_meta_object(
        &mut self,
        node: &RakuAstNode,
    ) -> Result<Value, RuntimeError> {
        if let Some(declared) = declared_ahead::declared_type(node) {
            return Ok(declared);
        }
        let Some(prelude) = declared_ahead::prelude_text(node) else {
            return Err(RuntimeError::new(format!(
                "RakuAST: the meta-object of a `{}` nested in another declaration, or built \
                 without a source, is only known once its unit is EVALed",
                node.class.printed_name()
            )));
        };
        // TODO: rakudo creates the type at BEGIN time inside the unit's own
        // lexical scope, so its methods close over the unit's `my` variables.
        // mutsu has no BEGIN-time phase for a tree: the declaration runs in a
        // unit of its own (the prelude plus the declaration), and a method
        // reading a `my` of the original unit does not compile here.
        let unit = self.str_ast_with_paths(&prelude, false)?;
        let ValueView::RakuAst(unit) = unit.view() else {
            return Err(RuntimeError::new("RakuAST: the prelude of a declaration is no unit"));
        };
        let mut unit = unit.clone();
        unit.fields.push(RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(crate::rakuast::as_statement(&Value::rakuast(
                Box::new(node.clone()),
            ))),
        });
        let declared = self.eval_rakuast_node(&Value::rakuast(Box::new(unit)))?;
        declared_ahead::record_declared_type(node, &declared);
        Ok(declared)
    }

    /// After an `EVAL` of `tree`: record the type each top-level package
    /// declaration of it declared, so its `meta-object` answers that type
    /// rather than declaring another (ADR-17472).
    // Cost: O(s), s = top-level statements of `tree`.
    pub(crate) fn record_evaluated_package_types(&self, tree: &RakuAstNode) {
        for node in declared_ahead::toplevel_packages(tree) {
            if declared_ahead::declared_type(&node).is_some() {
                continue;
            }
            let Some(name) = declared_ahead::package_name(&node) else {
                continue;
            };
            let qualified = if self.current_package_is_global_name() {
                None
            } else {
                Some(crate::qualified::qualified_text(self.current_package_str(), &name))
            };
            let found = qualified
                .into_iter()
                .map(|q| q.as_str().to_string())
                .chain(std::iter::once(name))
                .find(|candidate| self.has_type_or_package_kind(candidate));
            if let Some(found) = found {
                declared_ahead::record_declared_type(
                    &node,
                    &Value::package(crate::symbol::Symbol::intern(&found)),
                );
            }
        }
    }
}
