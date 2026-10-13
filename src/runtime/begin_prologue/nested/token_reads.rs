//! What a `token`, `rule` or `regex` of an inner scope reads when it matches:
//! the code its pattern runs and the variables and routines it interpolates
//! (ADR-0134, slice 2; [`super::routine_copy::copyable_token`]).
//!
//! A compiled token declaration does not hold its pattern's code blocks, so
//! [`Scan::of`] on the declaration sees none of them. The pattern's regex tree
//! does: each code block, predicate and interpolated block is a list of
//! statements, and each interpolation names its variable. Those are scanned
//! like a routine body, so a lifted BEGIN gets the cells of the variables the
//! token closes over.

use super::scan::Scan;
use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_regex_node};
use crate::regex_tree::{QuantifierKind, RegexNode, RegexTree};
use crate::symbol::Symbol;

/// The code and names one token's pattern reads.
#[derive(Default)]
pub(super) struct TokenReads {
    /// Each code block of the pattern as a block of its own, and each
    /// argument expression of a subrule or callable.
    code: Vec<Stmt>,
    /// The interpolated variables (`$x`, `<@alts>`, `<&rule>`), keyed the
    /// way the compiler names a free variable ([`env_key`]).
    vars: Vec<String>,
}

/// A variable's name as the compiler lists it among the free variables: a
/// scalar without its sigil, any other with it.
// Cost: O(n), n = the length of `name`.
fn env_key(sigil: char, name: &str) -> String {
    if sigil == '$' {
        name.to_string()
    } else {
        format!("{sigil}{name}")
    }
}

impl TokenReads {
    /// The reads of `pattern`, or `None` when its regex tree is not known
    /// statically or it declares a variable (`:my $x`), which the scan of a
    /// code block cannot tell from a read of the scope's.
    // Cost: O(p), p = the length of the pattern.
    pub(super) fn of(pattern: &str) -> Option<TokenReads> {
        if pattern.contains(":my") || pattern.contains(":our") || pattern.contains(":state") {
            return None;
        }
        let tree = RegexTree::parse_static(pattern, true)?;
        let mut reads = TokenReads::default();
        reads.visit_regex_node(&tree.body);
        Some(reads)
    }

    /// The scan of everything the pattern reads.
    // Cost: O(c), c = the size of the pattern's code.
    pub(super) fn scan(&self) -> Scan {
        let mut scan = Scan::of(&self.code);
        for var in &self.vars {
            let sym = Symbol::intern(var);
            if !scan.free.contains(&sym) {
                scan.free.push(sym);
            }
        }
        scan
    }
}

impl<'ast> Visit<'ast> for TokenReads {
    fn visit_regex_node(&mut self, node: &'ast RegexNode) {
        match node {
            RegexNode::CodeAssertion { body, .. }
            | RegexNode::CodeBlock { body, .. }
            | RegexNode::InterpolatedBlock { body, .. } => {
                self.code.push(Stmt::Block(body.clone()));
            }
            RegexNode::Interpolation { name, .. } => self.vars.push(name.clone()),
            RegexNode::RegexValueInterpolation { name, sigil, .. } => {
                self.vars.push(env_key(*sigil, name));
            }
            RegexNode::ArrayInterpolation { name, .. } | RegexNode::ArrayLookaround { name, .. } => {
                self.vars.push(format!("@{name}"));
            }
            RegexNode::Quantified { atom, quantifier } => {
                self.visit_regex_node(atom);
                if let QuantifierKind::Block(code) = &quantifier.kind {
                    self.code.push(Stmt::Block(code.body.clone()));
                }
                if let Some(separator) = &quantifier.separator {
                    self.visit_regex_node(&separator.node);
                }
            }
            RegexNode::Callable { name, .. } => {
                self.vars.push(format!("&{name}"));
                walk_regex_node(self, node);
            }
            _ => walk_regex_node(self, node),
        }
    }

    // Any other code the pattern holds, one statement at a time: a name one
    // of them declares and another reads is then free, which only refuses
    // the lift.
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        self.code.push(Stmt::Block(vec![stmt.clone()]));
    }

    // A subrule's or callable's argument.
    fn visit_expr(&mut self, expr: &'ast Expr) {
        self.code.push(Stmt::Expr(expr.clone()));
    }
}

#[cfg(test)]
mod tests {
    use super::TokenReads;

    #[test]
    fn interpolations_are_read_by_their_free_variable_name() {
        let r = TokenReads::of(":ratchet $w ").expect("static");
        assert_eq!(r.vars, vec!["w".to_string()]);
        let r = TokenReads::of(":ratchet <@alts> @more <&rule>").expect("static");
        assert_eq!(r.vars, ["@alts", "@more", "&rule"]);
    }
}
