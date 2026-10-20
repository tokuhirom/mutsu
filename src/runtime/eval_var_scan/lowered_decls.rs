//! Declarations the lowered AST does not spell as `my`: a regex's `:my`
//! statements and a destructuring pointy block's parameters.

use super::*;
use crate::ast::SourceForm;

impl UndeclaredVars<'_> {
    /// Declares the parameters of the pointy `given`/`with`/`if` whose
    /// lowered body is `body`. The lowering binds a destructured parameter
    /// (`-> ($a, |rest)`) by assignment, so only its source form names it.
    // Cost: O(b + p), b = statements of `body`, p = parameters.
    pub(super) fn declare_pointy_params(&mut self, body: &[Stmt]) {
        let mut names = ParamNames::default();
        for stmt in body {
            if let Stmt::SourceForm(form) = stmt {
                match form.as_ref() {
                    SourceForm::GivenPointy { param_def, .. } => names.visit_param(param_def),
                    SourceForm::WithPointy {
                        param_def: Some(param_def),
                        ..
                    } => names.visit_param(param_def),
                    SourceForm::IfPointy { param_defs, .. } => {
                        for param in param_defs {
                            names.visit_param(param);
                        }
                    }
                    _ => {}
                }
            }
        }
        for name in &names.0 {
            self.declare(name);
        }
    }

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

/// The names of the parameters (and sub-signature parameters) visited.
#[derive(Default)]
struct ParamNames(Vec<String>);

impl<'ast> Visit<'ast> for ParamNames {
    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if matches!(kind, NameKind::Param | NameKind::BlockParam) {
            self.0.push(name.to_string());
        }
    }

    // A default or `where` is not a declaration.
    fn visit_expr(&mut self, _expr: &'ast Expr) {}
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
