//! Recognizing `%h{$k} ~= rhs` / `@a[$i] ~= rhs` for the fused element append
//! (`IndexAssignExprNamed { concat_append: true, .. }`, #9141).
//!
//! The parser desugars an element `~=` into
//! `do { my $__mutsu_idx_N = <key>; %h{$__mutsu_idx_N} = <EmptyStr-seeded
//! %h{$__mutsu_idx_N}> ~ rhs }`, so the store's index and the read's index are
//! the same temp variable, and the read is wrapped in the METAOP_ASSIGN
//! `EmptyStr` identity exactly as for a scalar `~=`. Only that shape is fused:
//! a literal `%h<k> = %h<k> ~ rhs` has no identity wrapper and keeps its own
//! (undefined-LHS warning) semantics.

use super::*;

impl Compiler {
    /// When `value` is the element-`~=` desugaring for the store
    /// `target[index] = value`, return the seeded element read and the RHS.
    pub(super) fn index_concat_append_parts<'a>(
        target: &Expr,
        index: &Expr,
        value: &'a Expr,
        outer_positional: bool,
    ) -> Option<(&'a Expr, &'a Expr)> {
        let Expr::Binary {
            left: seeded,
            op: TokenKind::Tilde,
            right: rhs,
        } = value
        else {
            return None;
        };
        let Expr::Unary {
            op: TokenKind::MetaAssignIdentity(crate::token_kind::MetaAssignIdentity::EmptyStr),
            expr: read,
        } = seeded.as_ref()
        else {
            return None;
        };
        // The read's own `is_positional` is not compared: the desugaring marks
        // an `@a[$i] ~=` read associative, and the read is compiled unchanged
        // either way. The target's sigil and the store's flag decide.
        let Expr::Index {
            target: read_target,
            index: read_index,
            ..
        } = read.as_ref()
        else {
            return None;
        };
        // Only a plain `%`/`@` variable read and written through the SAME
        // desugar temp index: that is what guarantees the read and the store
        // address one element.
        let same_target = match (target, read_target.as_ref()) {
            (Expr::HashVar(a), Expr::HashVar(b)) => !outer_positional && a == b,
            (Expr::ArrayVar(a), Expr::ArrayVar(b)) => outer_positional && a == b,
            _ => false,
        };
        let same_index = matches!(
            (index, read_index.as_ref()),
            (Expr::Var(a), Expr::Var(b)) if a == b && a.starts_with("__mutsu_idx_")
        );
        (same_target && same_index).then_some((seeded.as_ref(), rhs.as_ref()))
    }
}
