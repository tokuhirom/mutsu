//! The compile-time placeholder of a statically named `require`.
//!
//! Rakudo declares `require Foo`'s target as a stub package in the lexical
//! scope the statement sits in while it parses the statement, so the name
//! resolves from the head of that scope on, and stays resolvable when the load
//! fails:
//!
//! ```raku
//! say ::('Foo').^name;      # Foo
//! try require Foo;          # no such module: the stub stays
//! ```
//!
//! A computed target (`require ::($name)`) names nothing until it runs, so it
//! gets no placeholder, and neither does a file path.
//!
//! mutsu hoists the same declaration to the entry of the owning scope, next to
//! the scope's hoisted routines ([`Compiler::hoist_sub_decls`]). The opcode it
//! emits ([`OpCode::DeclareRequireStub`]) binds the placeholder as a lexical,
//! like a `my package`, so it dies with a block that declared it and never
//! shadows a name that already resolves.

use super::Compiler;
use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt};
use crate::opcode::OpCode;
use crate::value::{Value, ValueView};

/// The statically named `require` targets a scope's own statements make, in
/// source order and without repeats. Only the scope's own statements are
/// searched: a `require` in a nested block, loop body, routine or closure
/// belongs to that scope, which declares its own placeholder on entry.
// Cost: O(n), n = size of the statements' own expression trees (nested
// scopes excluded).
pub(crate) fn static_require_targets(stmts: &[Stmt]) -> Vec<String> {
    let mut scan = ScopeRequires::default();
    for stmt in stmts {
        scan.visit_stmt(stmt);
    }
    scan.targets
}

/// The walk of [`static_require_targets`] (ADR-0137 visitor).
#[derive(Default)]
struct ScopeRequires {
    targets: Vec<String>,
    /// How many expressions enclose the node being visited. A statement met
    /// inside an expression sits in a nested block, closure or `do`, whose
    /// `require` belongs to that scope.
    expr_depth: usize,
    /// Set by a statement-prefix `try` for the one statement it carries, which
    /// opens no scope.
    prefix_stmt: bool,
}

impl Visit for ScopeRequires {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        let prefix = std::mem::take(&mut self.prefix_stmt);
        if self.expr_depth > 0 && !prefix {
            return;
        }
        match stmt {
            // The statement kinds that run in the scope itself. Every other
            // kind opens a scope of its own, or is not a place a `require` can
            // sit, and is skipped.
            Stmt::Expr(_)
            | Stmt::VarDecl { .. }
            | Stmt::Assign { .. }
            | Stmt::Return(_)
            | Stmt::Die(_)
            | Stmt::Fail(_)
            | Stmt::Say(_)
            | Stmt::Put(_)
            | Stmt::Print(_)
            | Stmt::Note(_)
            // A parser desugaring that groups statements without a scope.
            | Stmt::SyntheticBlock(_)
            // The lowering of a postfix `if`/`unless`: it opens no block.
            | Stmt::If {
                is_statement_modifier: true,
                ..
            } => walk_stmt(self, stmt),
            _ => {}
        }
    }

    // A parameter default belongs to its closure's scope too.
    fn visit_param(&mut self, _param: &crate::ast::ParamDef) {}

    fn visit_expr(&mut self, expr: &Expr) {
        if let Expr::Call { name, args } = expr
            && name.resolve() == "require"
            && let Some(Expr::Literal(target)) = args.first()
            && let ValueView::Package(module) = target.view()
        {
            let module = module.resolve();
            if !self.targets.contains(&module) {
                self.targets.push(module);
            }
        }
        // `try STMT` carries its statement as the one bare `Stmt::Expr` of the
        // body. A braced `try { ... }` is a block of its own: its statements
        // start with a line marker.
        self.prefix_stmt = matches!(
            expr,
            Expr::Try { body, .. } if matches!(body.as_slice(), [Stmt::Expr(_)])
        );
        self.expr_depth += 1;
        walk_expr(self, expr);
        self.expr_depth -= 1;
        self.prefix_stmt = false;
    }
}

impl Compiler {
    /// Declare, on entry to the scope whose statements are `stmts`, the
    /// placeholder of every statically named `require` the scope makes (see the
    /// module doc comment).
    // Cost: O(n) to scan, n = size of the scope's own statements; the emitted
    // ops are one per distinct target.
    pub(super) fn hoist_require_stubs(&mut self, stmts: &[Stmt]) {
        for target in static_require_targets(stmts) {
            let name_idx = self.code.add_constant(Value::str(target));
            self.code.emit(OpCode::DeclareRequireStub { name_idx });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn requires(src: &str) -> Vec<String> {
        let stmts = crate::parse_dispatch::parse_source(src)
            .map(|(stmts, _)| stmts)
            .unwrap();
        static_require_targets(&stmts)
    }

    #[test]
    fn a_require_anywhere_in_the_statement_expression_is_found() {
        assert_eq!(requires("my $x = (require Foo);"), vec!["Foo"]);
        assert_eq!(requires("my %h = a => (require Foo);"), vec!["Foo"]);
        assert_eq!(requires("f(:x(require Foo));"), vec!["Foo"]);
        assert_eq!(requires("say (try require Foo).defined;"), vec!["Foo"]);
    }

    #[test]
    fn a_statement_prefix_try_and_a_postfix_condition_open_no_scope() {
        assert_eq!(requires("try require Foo;"), vec!["Foo"]);
        assert_eq!(requires("require Foo if $x;"), vec!["Foo"]);
        assert_eq!(requires("say 1 if try require Foo;"), vec!["Foo"]);
    }

    #[test]
    fn a_require_in_a_nested_block_or_closure_belongs_to_that_scope() {
        assert!(requires("my $c = { require Foo };").is_empty());
        assert!(requires("my $c = -> $x = (require Foo) { };").is_empty());
        assert!(requires("my $x = do { require Foo };").is_empty());
        assert!(requires("try { require Foo }").is_empty());
        assert!(requires("{ require Foo }").is_empty());
        assert!(requires("if $x { require Foo }").is_empty());
    }

    #[test]
    fn a_computed_target_or_a_file_path_names_no_stub() {
        assert!(requires("require ::($name);").is_empty());
        assert!(requires("require 'lib/Foo.rakumod';").is_empty());
        assert!(requires("require $path;").is_empty());
    }

    #[test]
    fn a_repeated_target_is_declared_once() {
        assert_eq!(requires("try require Foo; try require Foo;"), vec!["Foo"]);
    }
}
