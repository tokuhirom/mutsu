//! Whether an attribute default may read the object's attributes.
//!
//! A default runs with `$!x`/`$.x` bound in its environment for every
//! attribute of the object under construction
//! (`Interpreter::eval_attr_default_expr`). Binding them costs O(attributes)
//! per default, so a class with many attributes and several computed
//! defaults paid O(attributes x defaults) per `.new` (LLM::Chat's `Response`,
//! ~31 attributes, spent ~40% of its construction there). A default such as
//! `Lock.new`, `now` or `[]` names no attribute. When the scan proves that,
//! the default skips the binding.
//!
//! The scan is a whitelist (ADR-0137 visitor): a default counts as
//! attribute-free only when every node is a shape that cannot reach the
//! attribute bindings other than by naming a `!`/`.`-twigil variable. A block or
//! closure, an interpolating string, a regex, `EVAL` or an indirect name
//! lookup can, so any of them makes the answer "may read".

use crate::ast::Expr;

/// Whether `expr` may read an attribute of the object under construction
/// through its `$!x`/`$.x` (or `@!`, `%!`, `&!`) bindings.
// Cost: O(n), n = expression size.
pub(crate) fn may_read_attributes(expr: &Expr) -> bool {
    let mut scan = AttrRefScan { may_read: false };
    crate::ast_visit::Visit::visit_expr(&mut scan, expr);
    scan.may_read
}

struct AttrRefScan {
    may_read: bool,
}

impl<'ast> crate::ast_visit::Visit<'ast> for AttrRefScan {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.may_read {
            return;
        }
        match expr {
            Expr::Var(name)
            | Expr::ArrayVar(name)
            | Expr::HashVar(name)
            | Expr::CodeVar(name) => {
                if name.starts_with(['!', '.']) {
                    self.may_read = true;
                }
            }
            Expr::Call { name, .. } if matches!(name.resolve().as_str(), "EVAL" | "EVALFILE") => {
                self.may_read = true;
            }
            Expr::Literal(_)
            | Expr::BareWord(_)
            | Expr::Call { .. }
            | Expr::MethodCall { .. }
            | Expr::Binary { .. }
            | Expr::Unary { .. }
            | Expr::Ternary { .. }
            | Expr::Index { .. }
            | Expr::ArrayLiteral(_)
            | Expr::BracketArray(..)
            | Expr::PositionalPair(_)
            | Expr::Hash(..) => crate::ast_visit::walk_expr(self, expr),
            _ => self.may_read = true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn default_of(src: &str) -> Expr {
        let (stmts, _) = crate::parser::parse_program(src).expect("parses");
        let mut found = None;
        for stmt in &stmts {
            if let crate::ast::Stmt::ClassDecl { body, .. } = stmt {
                for s in body {
                    if let crate::ast::Stmt::HasDecl {
                        default: Some(d), ..
                    } = s
                    {
                        found = Some(d.clone());
                    }
                }
            }
        }
        found.expect("a default")
    }

    #[test]
    fn plain_defaults_read_no_attribute() {
        assert!(!may_read_attributes(&default_of("class A { has $.l = Lock.new }")));
        assert!(!may_read_attributes(&default_of("class A { has @.a = [1, 2] }")));
        assert!(!may_read_attributes(&default_of("class A { has $.n = now }")));
    }

    #[test]
    fn attribute_references_and_opaque_shapes_may_read() {
        assert!(may_read_attributes(&default_of("class A { has $.x = 1; has $.y = $!x + 1 }")));
        assert!(may_read_attributes(&default_of("class A { has $.x = 1; has &.c = { $!x } }")));
        assert!(may_read_attributes(&default_of(
            "class A { has $.x = 1; has $.s = \"v$!x\" }"
        )));
        assert!(may_read_attributes(&default_of("class A { has $.e = EVAL('1') }")));
    }
}
