use crate::symbol::Symbol;

/// The env keys `!name` and `.name` through which an attribute default reads
/// attribute `name`, built once per name: a default is evaluated once per
/// attribute per construction and installed a pair per attribute of the class
/// each time, so the two `format!`s and their interns were ~1000 string builds
/// for a 22-attribute class (`Red::Column.new`).
// Cost: O(1) expected after the first call for a name; one probe.
pub(super) fn attr_env_keys(name: Symbol) -> (Symbol, Symbol) {
    thread_local! {
        static KEYS: std::cell::RefCell<rustc_hash::FxHashMap<Symbol, (Symbol, Symbol)>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    if let Some(keys) = KEYS.with(|memo| memo.borrow().get(&name).copied()) {
        return keys;
    }
    let text = name.as_str();
    let keys = (
        Symbol::intern(&format!("!{text}")),
        Symbol::intern(&format!(".{text}")),
    );
    KEYS.with(|memo| memo.borrow_mut().insert(name, keys));
    keys
}
