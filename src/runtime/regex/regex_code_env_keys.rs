//! The env keys a regex code block's body reads its captures under (`$0` as
//! `0`, `$<name>` as `<name>`, `$/`, `$¢`), as symbols interned once per
//! thread instead of formatted and re-interned on every block run (#12917).

use crate::symbol::Symbol;
use std::cell::RefCell;

thread_local! {
    /// `0`, `1`, ...: the key of `$N`, grown on first use of each index.
    static POSITIONAL: RefCell<Vec<Symbol>> = const { RefCell::new(Vec::new()) };
    /// Capture name -> its `<name>` key.
    static NAMED: RefCell<rustc_hash::FxHashMap<Symbol, Symbol>> =
        RefCell::new(rustc_hash::FxHashMap::default());
}

/// The env key of the positional capture variable `$i`.
// Cost: O(1) amortized; the first use of an index interns it.
pub(super) fn positional_var_sym(i: usize) -> Symbol {
    POSITIONAL.with(|cell| {
        let mut syms = cell.borrow_mut();
        while syms.len() <= i {
            let next = syms.len();
            syms.push(Symbol::intern(&next.to_string()));
        }
        syms[i]
    })
}

/// The env key of the named capture variable `$<name>`, `<name>`.
// Cost: O(1) expected; the first use of a name interns its key.
pub(super) fn named_capture_var_sym(name: &Symbol) -> Symbol {
    NAMED.with(|cell| {
        *cell
            .borrow_mut()
            .entry(*name)
            .or_insert_with(|| Symbol::intern(&format!("<{name}>")))
    })
}

/// The env key of `$¢`, the current match state.
// Cost: O(1).
pub(super) fn cursor_var_sym() -> Symbol {
    static SYM: std::sync::OnceLock<Symbol> = std::sync::OnceLock::new();
    *SYM.get_or_init(|| Symbol::intern("\u{00A2}"))
}

/// The env key of `$/`.
// Cost: O(1).
pub(super) fn match_var_sym() -> Symbol {
    static SYM: std::sync::OnceLock<Symbol> = std::sync::OnceLock::new();
    *SYM.get_or_init(|| Symbol::intern("/"))
}
