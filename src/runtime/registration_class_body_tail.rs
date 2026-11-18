//! The per-statement publication tail of the class-body walk
//! (`run_class_body`): after each body op, record any class-scoped routines it
//! registered and publish the class definition so far.

use super::registration_class_body::ClassBodyCx;
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Publish what body op `op` added to the class `cx.name`: the routines a
    /// class-body `sub` or `use` registered under the class package, and the
    /// class definition as it stands. Not run on a per-entry re-entry, which
    /// changes nothing about the class (#18070).
    // Cost: O(f), f = routines in the routine table (a key scan for a `sub`
    // or `use` op), plus one class-table write.
    pub(super) fn publish_class_body_op(
        &mut self,
        cx: &ClassBodyCx<'_>,
        op: &crate::opcode::ClassBodyOp,
        saved_functions_keys: &HashSet<String>,
    ) {
        // Check if any new functions were registered under the class package
        // during body processing (e.g., class-scoped subs).
        // Only count functions that are actual subs (not methods, which are
        // registered via MethodDecl and stored in the class methods table).
        //
        // A `multi sub` does NOT register under the bare `Pkg::name`
        // key: `insert_multi_overload` keys its candidates
        // `Pkg::name/arity:types` (plus `__mN` for same-signature
        // siblings). Probing only the exact key therefore missed every
        // class-body `proto`+`multi` family, `has_class_scoped_subs`
        // stayed false, and method dispatch left `current_package` at
        // GLOBAL -- so `bare_name_packages()` inside a method never
        // reached the class and the family was unreachable from the
        // class's own methods ("Unknown function"). Accept the multi
        // key shape as well.
        if let crate::opcode::ClassBodyOp::ClassSub { name: sub_name, .. } = op {
            let fq = crate::qualified::qualified_text(cx.name, sub_name)
                .as_str()
                .to_string();
            let multi_prefix = format!("{fq}/");
            let registered = {
                let registry = self.routines();
                registry.functions.keys().any(|k| {
                    let ks = k.resolve();
                    (ks == fq || ks.starts_with(&multi_prefix))
                        && !saved_functions_keys.contains(&ks)
                })
            };
            if registered {
                self.registry_mut()
                    .class_subs
                    .entry(cx.name.to_string())
                    .or_default()
                    .insert(fq, Value::TRUE);
            }
        } else if matches!(
            op,
            crate::opcode::ClassBodyOp::Other {
                raw: Stmt::Use { .. } | Stmt::Import { .. },
                ..
            }
        ) {
            // A class-body `use`/`import` installs its exports under
            // THIS class's own package (`import_module`'s `target_pkg`
            // is `current_package()`, already the class while its body
            // runs) -- but there is no single known sub name to probe
            // for, unlike a `sub` declaration above. Diff the whole
            // function-key set instead: any key newly registered under
            // `cx.name::` is one of the statement's imports. Without
            // this, `has_class_scoped_subs` stayed false for a class
            // whose only class-scoped routine came from a `use`
            // (rather than its own `sub`), so method dispatch never
            // anchored `current_package` to the class and an imported
            // sub was "Unknown function" from the class's own methods
            // (#8883).
            let prefix = format!("{}::", cx.name);
            let new_keys: Vec<String> = {
                let registry = self.routines();
                registry
                    .functions
                    .keys()
                    .filter_map(|k| {
                        let ks = k.resolve();
                        (ks.starts_with(&prefix) && !saved_functions_keys.contains(&ks))
                            .then_some(ks)
                    })
                    .collect()
            };
            if !new_keys.is_empty() {
                let mut registry = self.registry_mut();
                let entry = registry.class_subs.entry(cx.name.to_string()).or_default();
                for fq in new_keys {
                    entry.insert(fq, Value::TRUE);
                }
            }
        }
        self.registry_mut()
            .classes
            .insert(cx.name.to_string(), cx.class_def.clone().into());
        self.registry_mut()
            .sync_accessor_entries(Symbol::intern(cx.name));
    }
}
