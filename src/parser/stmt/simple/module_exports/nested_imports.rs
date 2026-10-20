//! Re-export candidates belong to a module scan, not the importer's lexical scope.

use std::cell::RefCell;
use std::collections::HashSet;

use super::{InlineModuleExport, ModuleScanResult};

#[derive(Default)]
pub(super) struct Imports {
    pub(super) exports: Vec<InlineModuleExport>,
    pub(super) value_terms: HashSet<String>,
}

thread_local! {
    static IMPORTS: RefCell<Imports> = RefCell::new(Imports::default());
}

// Cost: O(e + v), e = routines, v = value terms in the imported scan.
pub(super) fn record(scan: &ModuleScanResult) {
    IMPORTS.with(|imports| {
        let mut imports = imports.borrow_mut();
        imports.exports.extend(scan.exports.iter().cloned());
        imports.value_terms.extend(scan.value_terms.iter().cloned());
    });
}

/// A nested module parse restores its parent's collection on every exit path.
pub(super) fn collect<T>(parse: impl FnOnce() -> T) -> (T, Imports) {
    struct Restore(Option<Imports>);
    impl Drop for Restore {
        fn drop(&mut self) {
            if let Some(previous) = self.0.take() {
                IMPORTS.with(|imports| *imports.borrow_mut() = previous);
            }
        }
    }
    let mut restore = Restore(Some(
        IMPORTS.with(|imports| std::mem::take(&mut *imports.borrow_mut())),
    ));
    let result = parse();
    let nested = IMPORTS
        .with(|imports| std::mem::replace(&mut *imports.borrow_mut(), restore.0.take().unwrap()));
    (result, nested)
}
