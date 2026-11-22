//! The scalars a regex literal's own code may write while it matches.
//!
//! A regex's inline code (`{ ... }`, `<?{ ... }>`, `<{ ... }>`, a quantifier
//! block) is not compiled into the enclosing chunk's ops, so the op scan of
//! [`super::CompiledCode`]'s capture analysis cannot see its writes. A closure
//! that captures such a scalar (above all the regex's own `"..."` qq thunk,
//! `crate::regex_qq_atoms`) would then snapshot it by value and miss a write
//! the same match made (#18630). Every scalar the code names is counted as
//! possibly written: the code is evaluated by name against the matching
//! frame, and a read-only name only costs a shared cell when a closure also
//! captures it.

use crate::ast_visit::{NameKind, Visit, walk_regex_node};
use crate::regex_tree::{QuantifierKind, RegexNode, RegexTree};
use crate::symbol::Symbol;

/// The scalar names (stored sigil-less, as `locals` key them) that the code
/// blocks of `tree` mention.
// Cost: O(t), t = the size of the regex tree including its code blocks.
pub(super) fn regex_code_scalar_names(tree: &RegexTree) -> Vec<Symbol> {
    let mut scan = CodeNames::default();
    scan.visit_regex_node(&tree.body);
    scan.names
}

#[derive(Default)]
struct CodeNames {
    /// How many code blocks the walk is inside.
    in_code: usize,
    names: Vec<Symbol>,
}

impl<'ast> Visit<'ast> for CodeNames {
    fn visit_regex_node(&mut self, node: &'ast RegexNode) {
        match node {
            RegexNode::CodeAssertion { body, .. }
            | RegexNode::CodeBlock { body, .. }
            | RegexNode::InterpolatedBlock { body, .. } => {
                self.in_code += 1;
                self.visit_stmts(body);
                self.in_code -= 1;
            }
            RegexNode::Quantified { atom, quantifier } => {
                self.visit_regex_node(atom);
                if let QuantifierKind::Block(code) = &quantifier.kind {
                    self.in_code += 1;
                    self.visit_stmts(&code.body);
                    self.in_code -= 1;
                }
                if let Some(separator) = &quantifier.separator {
                    self.visit_regex_node(&separator.node);
                }
            }
            _ => walk_regex_node(self, node),
        }
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if self.in_code == 0 || !matches!(kind, NameKind::Var | NameKind::AssignTarget) {
            return;
        }
        if !crate::env::is_plain_user_lexical(name) || name.starts_with(['@', '%', '&', '$']) {
            return;
        }
        let sym = Symbol::intern(name);
        if !self.names.contains(&sym) {
            self.names.push(sym);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn names(pattern: &str) -> Vec<String> {
        let tree = RegexTree::parse_static(pattern, false).expect("static tree");
        regex_code_scalar_names(&tree)
            .into_iter()
            .map(|s| s.resolve().to_string())
            .collect()
    }

    #[test]
    fn code_block_names_are_listed_and_atoms_are_not() {
        assert_eq!(names(r#" x { $s = "b" } "$t" $u "#), vec!["s"]);
        assert_eq!(names(r#" <?{ $n++ }> "#), vec!["n"]);
        assert!(names(" abc ").is_empty());
    }
}
