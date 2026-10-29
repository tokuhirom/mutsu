use super::*;

/// Remembers, for the duration of one element loop, the class of an instance
/// that already satisfied the loop's constraint.
///
/// A typed container attribute (`has Trieish %.children`) checks every element
/// of an assigned hash against one constraint, and each check resolves the
/// constraint name again (a role lookup is ~5k Ir). For an `Instance` element
/// and a constraint that looks at nothing but the type, the verdict is a
/// function of the instance's class, so the second and later instances of that
/// class skip the check. Only a *pass* is remembered, and only inside one loop,
/// so the frame and registry the check resolves against cannot change under it.
#[derive(Default)]
pub(crate) struct ElemTypeMemo {
    ok_class: Option<Symbol>,
    value_dependent: Option<bool>,
}

impl Interpreter {
    /// [`Self::type_matches_value`] with the per-loop class memo of
    /// [`ElemTypeMemo`].
    // Cost: O(1) on a memo hit; otherwise as `type_matches_value`.
    pub(crate) fn type_matches_value_memo(
        &mut self,
        constraint: &str,
        value: &Value,
        memo: &mut ElemTypeMemo,
    ) -> bool {
        let ValueView::Instance { class_name, .. } = value.view() else {
            return self.type_matches_value(constraint, value);
        };
        if memo.ok_class == Some(class_name) {
            return true;
        }
        let ok = self.type_matches_value(constraint, value);
        if ok {
            let value_dependent = match memo.value_dependent {
                Some(v) => v,
                None => {
                    let v = self.type_constraint_is_value_dependent(constraint);
                    memo.value_dependent = Some(v);
                    v
                }
            };
            if !value_dependent {
                memo.ok_class = Some(class_name);
            }
        }
        ok
    }
}
