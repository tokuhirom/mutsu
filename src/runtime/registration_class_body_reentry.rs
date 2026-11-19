//! A class body that runs again (#18088).
//!
//! A `my class` declared in a routine runs its body once per entry of the
//! routine. The body's `my` statics live in the env while the body runs and
//! are persisted into `package_lexicals` when it ends
//! (`persist_class_body_statics`), where a method of the class reads them. A
//! read inside the body consults that store before the env
//! (`package_scope_lexical`), so on a second entry the statement after
//! `my $z = $i` read the value the previous entry persisted.

use super::*;

impl Interpreter {
    /// Drop the statics a previous run of the class body `name` persisted for
    /// the names this body declares, so the running body reads its own
    /// declarations. A body that never ran before has nothing to drop, and
    /// the table is not written then.
    // Cost: O(s), s = statics the body declares.
    pub(super) fn forget_persisted_class_body_statics(
        &mut self,
        name: &str,
        declared_static_names: &[Symbol],
    ) {
        if declared_static_names.is_empty() {
            return;
        }
        let persisted = self
            .lexicals
            .package_lexicals
            .read()
            .get(name)
            .is_some_and(|store| {
                declared_static_names
                    .iter()
                    .any(|static_name| static_name.with_str(|n| store.contains_key(n)))
            });
        if !persisted {
            return;
        }
        self.package_lexicals_write(|table| {
            if let Some(store) = table.get_mut(name) {
                for static_name in declared_static_names {
                    static_name.with_str(|n| store.remove(n));
                }
            }
        });
    }
}
