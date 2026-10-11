//! Routines a block-scoped `use` imported, resolved for code nested in that
//! block after the block's dynamic import scope has popped (#8751).

use super::*;

impl Interpreter {
    // Cost: O(m), m = modules `use`d by blocks enclosing `code` (usually 0).
    /// The registry key of the exported routine `name` in one of the modules
    /// `code` lexically sits under, if any. Rakudo installs a `use`'s symbols in
    /// the enclosing lexpad, so the import wins over a same-named routine of
    /// the importing unit and outlives the block's dynamic extent.
    // TODO: bind the import lexically in the block's frame (ADR-12529 phase 2)
    // instead of probing the registry by module name at call time.
    pub(super) fn lexical_use_export_key(&mut self, code: &CompiledCode, name: &str) -> Option<String> {
        for module in &code.lexical_use_modules {
            let key = format!("{}::EXPORT::DEFAULT::{name}", module.as_str());
            if self.has_function(&key) {
                return Some(key);
            }
        }
        None
    }
}
