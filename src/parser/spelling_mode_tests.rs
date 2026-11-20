//! An executing parse builds no `Expr::Spelled`; a `.AST` parse
//! (`term_origins::Wanted`) does (ADR-15760 §2.4, §2.5 item 1).

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr};

#[derive(Default)]
struct Spellings(usize);

impl<'ast> Visit<'ast> for Spellings {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if matches!(expr, Expr::Spelled(_)) {
            self.0 += 1;
        }
        walk_expr(self, expr);
    }
}

fn spellings(stmts: &[Stmt]) -> usize {
    let mut found = Spellings::default();
    found.visit_stmts(stmts);
    found.0
}

/// Word lists, a negated infix, a `;` that ends no statement, a heredoc and a
/// compiler variable: each is spelled in a `.AST` parse.
const SOURCE: &str = "my @a = <a  b>; say 1 !== 2;; say $?LINE;\nsay q:to/E/;\n x\n E\n";

#[test]
fn an_executing_parse_builds_no_spellings() {
    let (stmts, _) = super::parse_program_unfinalized(SOURCE).expect("it parses");
    assert_eq!(spellings(&stmts), 0, "{stmts:?}");
    // No `Statement::Empty` placeholder either: the stray `;` is no statement.
    assert!(
        !stmts.iter().any(|s| matches!(s, Stmt::Expr(Expr::Literal(v)) if v.is_nil())),
        "{stmts:?}"
    );
}

#[test]
fn a_dot_ast_parse_keeps_its_spellings() {
    let _wanted = super::term_origins::Wanted::enter();
    let (stmts, _) = super::parse_program_unfinalized(SOURCE).expect("it parses");
    assert!(spellings(&stmts) >= 4, "{stmts:?}");
}
