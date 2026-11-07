//! Keeping the declarator docs of the routines an `EVAL` unit declares.
//!
//! The docs table is keyed by name and put back when the unit ends, so a
//! routine the unit returned would answer `.WHY` with `Nil` afterwards (rakudo
//! keeps the doc on the routine). [`Interpreter::pin_eval_routine_docs`] files
//! each such doc under the routine object's id, which the table swap leaves
//! alone.

use super::*;

impl Interpreter {
    /// The docs of the plain named routines (`&name`) the finished unit
    /// added or changed relative to `before`, as `.WHY` objects by routine id.
    // Cost: O(d), d = documented declarations of the unit.
    pub(super) fn pin_eval_routine_docs(
        &mut self,
        before: &HashMap<String, DocComment>,
        result: &Result<Value, RuntimeError>,
    ) -> Vec<(u64, Value)> {
        let mut pinned = Vec::new();
        let fresh: Vec<(String, DocComment)> = self
            .declarator_docs
            .doc_comments
            .iter()
            .filter(|(key, doc)| {
                key.starts_with('&')
                    && !key.contains('/')
                    && before.get(*key).is_none_or(|old| old.doc != doc.doc)
            })
            .map(|(key, doc)| (key.clone(), doc.clone()))
            .collect();
        for (key, doc) in fresh {
            // The routine is lexical to the unit, so it is reachable only
            // through what the unit answered.
            let Ok(routine) = result else { continue };
            let ValueView::Sub(data) = routine.view() else {
                continue;
            };
            if format!("&{}", data.name.resolve()) != key {
                continue;
            }
            let id = data.id;
            pinned.push((id, Self::make_pod_declarator(&doc.doc, routine.clone())));
        }
        pinned
    }
}
