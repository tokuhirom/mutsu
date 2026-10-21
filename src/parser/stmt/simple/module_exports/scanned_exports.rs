//! Recording a module's parse-time export scan (`scanned_exported_vars`,
//! `scanned_exported_types`), from a parsed `use` or, for a precompiled
//! module whose `use`s were not parsed, on demand (ADR-11756 §2.2).

use super::{ModuleScanResult, exported_types, exported_vars, scan_module_at};
use std::rc::Rc;

/// Record the variables and types the scan `scan` of `module` found it
/// exporting (`scanned_exported_vars`, `scanned_exported_types`).
// Cost: O(v + t), v / t = the module's exported variables / types.
pub(super) fn record_scanned_exports(module: &str, scan: &ModuleScanResult) {
    exported_vars::record(
        module,
        if scan.dynamic_export_stash {
            None
        } else if scan.declares_export_hook {
            // The hook's literal keys only (ADR-8751): what else the
            // module exports keeps the run-time import.
            Some(exported_vars::hook_vars(module, &scan.hook_value_keys))
        } else {
            Some(scan.exported_vars.as_slice().into())
        },
    );
    exported_types::record_hook_type_keys(module, &scan.hook_type_keys);
    if scan.unlisted_exported_vars || scan.declares_export_hook {
        exported_vars::record_incomplete(module);
    }
    exported_types::record(
        module,
        (!scan.declares_export_hook && !scan.dynamic_export_stash)
            .then(|| scan.exported_types.as_slice().into()),
    );
}

/// While a module unit's mainline is compiled or its cached compile is
/// validated, the scans of the modules its `use`s name (`modules`, each with
/// the source file the load resolves it to) are their canonical ones: what a
/// scan of the file finds, not whatever this thread's parse history left
/// (a `use` met under the recursion guard records nothing usable), so a
/// compile's [`crate::compiler::compile_inputs`] answer depends on the used
/// modules alone (ADR-11756 §2.2). Dropping the guard puts the thread's
/// entries back.
pub(crate) struct CanonicalScans {
    saved: Vec<(String, SavedEntry)>,
}

type SavedEntry = (
    Option<Option<Rc<[exported_vars::ExportedVar]>>>,
    Option<Option<Rc<[exported_types::ExportedType]>>>,
);

/// Install the canonical scans of `modules` until the guard drops.
// Cost: one scan per module (usually from the scan cache).
pub(crate) fn canonical_export_scans(
    modules: impl IntoIterator<Item = (String, Option<String>)>,
) -> CanonicalScans {
    let mut saved = Vec::new();
    for (module, path) in modules {
        if saved.iter().any(|(m, _)| *m == module) {
            continue;
        }
        let entry = (
            exported_vars::entry(&module),
            exported_types::entry(&module),
        );
        if module == "Test" {
            // `Test` exports routines only (`register_module_exports`).
            exported_vars::record(&module, Some(Rc::from([])));
            exported_types::record(&module, Some(Rc::from([])));
        } else if let Some(scan) = path.and_then(|path| scan_module_at(&module, path)) {
            record_scanned_exports(&module, &scan);
        } else {
            exported_vars::record(&module, None);
            exported_types::record(&module, None);
        }
        saved.push((module, entry));
    }
    CanonicalScans { saved }
}

impl Drop for CanonicalScans {
    fn drop(&mut self) {
        for (module, (vars, types)) in self.saved.drain(..) {
            exported_vars::restore_entry(&module, vars);
            exported_types::restore_entry(&module, types);
        }
    }
}
