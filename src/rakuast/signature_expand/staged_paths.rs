//! Where each target of a nested declaration group reads from the staged RHS.

use crate::ast::{Expr, SignatureDecl, SignatureGroup};
use crate::value::Value;

/// The position path of every flattened target of `decl`: its index in each
/// enclosing group, outermost first. Without a nested group that is the
/// target's own index; with one (`my ($a, ($b, $c))`), a leaf of a nested group
/// reads the element of the staged RHS element its group sits at.
// Cost: O(n), n = targets and groups.
pub(super) fn target_paths(decl: &SignatureDecl) -> Vec<Vec<usize>> {
    let Some(shape) = &decl.group_shape else {
        return (0..decl.vars.len()).map(|i| vec![i]).collect();
    };
    fn walk(members: &[SignatureGroup], prefix: &[usize], paths: &mut [Vec<usize>]) {
        for (position, member) in members.iter().enumerate() {
            let mut path = prefix.to_vec();
            path.push(position);
            match member {
                SignatureGroup::Var(index) => paths[*index] = path,
                SignatureGroup::Group(inner) => walk(inner, &path, paths),
            }
        }
    }
    let mut paths = vec![Vec::new(); decl.vars.len()];
    walk(shape, &[], &mut paths);
    paths
}

/// The staged list, or the nested element of it that `parent` names.
// Cost: O(d), d = nesting depth.
pub(super) fn staged_container(array_bare: &str, parent: &[usize]) -> Expr {
    parent
        .iter()
        .fold(Expr::ArrayVar(array_bare.to_string()), |target, &at| {
            Expr::Index {
                target: Box::new(target),
                index: Box::new(Expr::Literal(Value::int(at as i64))),
                is_positional: true,
                spelling: Default::default(),
            }
        })
}

/// The staged RHS element a target at `path` reads.
// Cost: O(d), d = nesting depth.
pub(super) fn staged_element(array_bare: &str, path: &[usize]) -> Expr {
    let Some((position, parent)) = path.split_last() else {
        return Expr::Literal(Value::NIL);
    };
    Expr::Index {
        target: Box::new(staged_container(array_bare, parent)),
        index: Box::new(Expr::Literal(Value::int(*position as i64))),
        is_positional: true,
        spelling: Default::default(),
    }
}
