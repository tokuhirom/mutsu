//! `import Module :TAG` at parse time.

use super::{INLINE_MODULE_EXPORTS, LOADING_MODULES, apply_scan_types, find_and_scan_module};

/// `import Module :TAG` of a module a `use` or `need` already loaded: the
/// tagged enum values the import selects become terms of the importer, as
/// they would under `use Module :TAG`. An inline module is handled by
/// [`super::import_inline_module_exports`]; a module no search path finds stays
/// unknown without tainting the type index (it may be declared later in the
/// same file).
pub(crate) fn register_import_tags(module: &str, tags: &[String]) {
    if INLINE_MODULE_EXPORTS.with(|m| m.borrow().contains_key(module))
        || LOADING_MODULES.with(|m| m.borrow().contains(module))
    {
        return;
    }
    if let Some(scan) = find_and_scan_module(module) {
        apply_scan_types(&scan, Some(tags));
    }
}
