//! The declaring frame's lexicals a role's deferred body names.
use super::*;

/// Every variable a subtree names, in env-key spelling: `$x` as `x`, `@a` as
/// `@a`, `%h` as `%h`, reads and assignment targets alike.
#[derive(Default)]
struct VarNames {
    names: Vec<Symbol>,
}

impl<'ast> crate::ast_visit::Visit<'ast> for VarNames {
    fn visit_name(&mut self, name: &str, kind: crate::ast_visit::NameKind) {
        use crate::ast_visit::NameKind;
        let sym = match kind {
            NameKind::Var | NameKind::AssignTarget => Symbol::intern(name),
            NameKind::ArrayVar => Symbol::intern(&format!("@{name}")),
            NameKind::HashVar => Symbol::intern(&format!("%{name}")),
            _ => return,
        };
        if !self.names.contains(&sym) {
            self.names.push(sym);
        }
    }
}

/// The outer-lexical slots a role's deferred body statements name.
///
/// The body runs only at composition, possibly in a frame that is not the
/// declaring one, and reaches a name it does not declare by name. Each of
/// these slots is boxed into a shared cell when the role is declared and put
/// in scope for every run of the body (`RoleDef::body_bind_cells`), so the
/// body reads and writes the declaration site's variable, not a same-named
/// one of the composing frame, and its writes need no return merge to reach
/// the declaring block (ADR-12529 §3, slice 34).
// Cost: O(n * s), n = names the body uses, s = the frame's lexical slots.
pub(super) fn role_body_outer_ref_slots<'a>(
    body_stmts: impl Iterator<Item = &'a Stmt>,
    outer_lexical_slots: &[(Symbol, u32)],
) -> Vec<u32> {
    let mut names = VarNames::default();
    for stmt in body_stmts {
        crate::ast_visit::Visit::visit_stmt(&mut names, stmt);
    }
    let mut slots = Vec::new();
    for name in names.names {
        if let Some((_, slot)) = outer_lexical_slots.iter().find(|(n, _)| *n == name)
            && !slots.contains(slot)
        {
            slots.push(*slot);
        }
    }
    slots
}
