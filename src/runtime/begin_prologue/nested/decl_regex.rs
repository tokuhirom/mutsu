//! The `BEGIN` statements of the code blocks of a `token`/`rule`/`regex`
//! declaration (#12812).
//!
//! A `BEGIN` runs once, while the unit is compiled, wherever it is written.
//! The code blocks of a declaration are scopes like a routine's, so the nested
//! lift walks them the same way. A declaration's matcher parses the pattern
//! *text*, not the tree (see `phasers::decl_regex`), so a lifted statement has
//! to leave the text as well: the text of each code block is scanned for its
//! `BEGIN` statements, and the ones the walk lifted, always the first of the
//! declaration's, are cut out of it.
//!
//! The lift needs the tree and the text to agree on what a `BEGIN` is, so a
//! declaration is only walked when every `BEGIN` in it is a statement of a
//! plain code block, and the scan finds as many in each block as the tree
//! holds. Any other declaration keeps its `BEGIN`s where they are.

use super::Walker;
use crate::ast::{Expr, PhaserKind, Stmt};
use crate::ast_visit::{Visit, VisitMut, walk_expr, walk_stmt};
use crate::regex_tree::{RegexNode, RegexTree};
use crate::runtime::phasers::decl_regex::{CodeRewriter, Edits, rewrite_pattern, statement_spans};
use crate::value::{Value, ValueView};

/// The `BEGIN`s found below a node, as statements and as expressions.
#[derive(Default)]
struct Census {
    statements: usize,
    expressions: usize,
}

impl<'ast> Visit<'ast> for Census {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if matches!(
            stmt,
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                ..
            }
        ) {
            self.statements += 1;
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if matches!(
            expr,
            Expr::PhaserExpr {
                kind: PhaserKind::Begin,
                ..
            }
        ) {
            self.expressions += 1;
        }
        walk_expr(self, expr);
    }
}

/// Each plain code block of a tree, in source order: its text and the number
/// of `BEGIN` statements in its body, nested blocks included.
#[derive(Default)]
struct Blocks(Vec<(String, usize)>);

impl<'ast> Visit<'ast> for Blocks {
    fn visit_regex_node(&mut self, node: &'ast RegexNode) {
        if let RegexNode::CodeBlock { code, body } = node {
            let mut census = Census::default();
            for stmt in body {
                census.visit_stmt(stmt);
            }
            self.0.push((code.clone(), census.statements));
        } else {
            crate::ast_visit::walk_regex_node(self, node);
        }
    }
}

/// The code blocks of `tree` when its `BEGIN`s can be lifted together with the
/// text of `pattern`.
// Cost: O(n), n = size of the declaration's tree and pattern text.
fn liftable_blocks(tree: &RegexTree, pattern: &str) -> Option<Vec<(String, usize)>> {
    let mut whole = Census::default();
    whole.visit_regex_node(&tree.body);
    if whole.statements == 0 || whole.expressions != 0 {
        return None;
    }
    let mut blocks = Blocks::default();
    blocks.visit_regex_node(&tree.body);
    let in_blocks: usize = blocks.0.iter().map(|(_, n)| n).sum();
    if in_blocks != whole.statements {
        return None;
    }
    if blocks
        .0
        .iter()
        .any(|(code, n)| statement_spans(code, &["BEGIN"]).len() != *n)
    {
        return None;
    }
    let unchanged: Edits = blocks
        .0
        .iter()
        .map(|(code, _)| Some((code.clone(), code.clone())))
        .collect();
    rewrite_pattern(pattern, &unchanged)?;
    Some(blocks.0)
}

/// `code` without its first `lifted` `BEGIN` statements.
// Cost: O(n), n = length of `code`.
fn without_first_begins(code: &str, lifted: usize) -> String {
    let mut out = String::with_capacity(code.len());
    let mut cursor = 0;
    for span in statement_spans(code, &["BEGIN"]).into_iter().take(lifted) {
        out.push_str(&code[cursor..span.start]);
        cursor = span.end;
    }
    out.push_str(&code[cursor..]);
    out
}

impl Walker<'_> {
    /// Lift the `BEGIN`s out of the code blocks of the `token`/`rule`/`regex`
    /// declaration `stmt`, if the tree and the text can be edited together.
    // Cost: O(n), n = size of the declaration's tree and pattern text.
    pub(super) fn lift_declaration_begins(&mut self, stmt: &mut Stmt) {
        let (Stmt::TokenDecl {
            params,
            body,
            source_regex,
            ..
        }
        | Stmt::RuleDecl {
            params,
            body,
            source_regex,
            ..
        }) = stmt
        else {
            return;
        };
        let Some(tree) = source_regex.as_ref() else {
            return;
        };
        let [Stmt::Expr(Expr::Literal(literal))] = body.as_mut_slice() else {
            return;
        };
        let ValueView::Regex(pattern) = literal.view() else {
            return;
        };
        let pattern = pattern.to_string();
        let Some(before) = liftable_blocks(tree, &pattern) else {
            return;
        };

        let mut lifted = tree.clone();
        self.frames.push(Self::params_frame(params.iter().cloned()));
        self.visit_regex_node_mut(&mut lifted.body);
        self.frames.pop();

        let mut after = Blocks::default();
        after.visit_regex_node(&lifted.body);
        if after.0.len() != before.len() {
            return;
        }
        let mut changed = false;
        let edits: Edits = before
            .iter()
            .zip(&after.0)
            .map(|((code, was), (_, now))| {
                let gone = was.saturating_sub(*now);
                changed |= gone > 0;
                (gone > 0).then(|| (code.clone(), without_first_begins(code, gone)))
            })
            .collect();
        if !changed {
            return;
        }
        let Some(new_pattern) = rewrite_pattern(&pattern, &edits) else {
            return;
        };
        CodeRewriter::new(&edits).visit_regex_node_mut(&mut lifted.body);
        let mut value = Value::regex(new_pattern).with_regex_source_tree(lifted.clone());
        if let Some(declared) = literal.regex_declared_source() {
            value = value.with_regex_declared_source(declared);
        }
        *literal = value;
        *source_regex = Some(lifted);
    }
}
