//! Declarations the lowered AST does not spell as `my`: a regex's `:my`
//! statements.

use super::*;

impl UndeclaredVars<'_> {
    /// Declares a regex's `:my` variables, which are in scope after the
    /// regex too, as in rakudo ("helps scoping the $c variable within the
    /// regex and beyond").
    // Cost: O(n), n = size of the regex tree.
    pub(super) fn declare_regex_vars(&mut self, node: &RegexNode) {
        let mut decls = RegexDecls::default();
        decls.visit_regex_node(node);
        for name in decls.0 {
            self.declare(name);
        }
    }
}

/// The variables a regex's `:my` statements declare, outside its code
/// blocks (whose declarations are their own).
#[derive(Default)]
struct RegexDecls<'ast>(Vec<&'ast str>);

impl<'ast> Visit<'ast> for RegexDecls<'ast> {
    fn visit_regex_node(&mut self, node: &'ast RegexNode) {
        if let RegexNode::Extension(crate::regex_tree::RegexExtension::Statement(code)) = node {
            for stmt in &code.body {
                if let Stmt::VarDecl { name, .. } = stmt {
                    self.0.push(name);
                }
            }
        }
        crate::ast_visit::walk_regex_node(self, node);
    }

    // A code block's statements are not the regex's declarations.
    fn visit_stmt(&mut self, _stmt: &'ast Stmt) {}

    fn visit_expr(&mut self, _expr: &'ast Expr) {}
}
