//! Export stashes whose keys are computed at run time (#9500).
//!
//! `OUR::{'&postfix:<' ~ $code ~ '>'} := sub { ... }` inside a module's
//! `my package EXPORT::<tag> { ... }` exports routines whose names exist only
//! once the module body has run. The static scan cannot list them, so it only
//! records *that* the module does this; a `use` of such a module then runs it
//! at parse time (`runtime::parse_time_exports`) to learn the names, as Rakudo
//! does by compiling `use` at BEGIN time. The binding itself is detected by the
//! module's declaration scan (`decl_scan.rs`), in any position of the stash.

use super::InlineModuleExport;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A probe's key: the module name and the search path it resolved under.
type ProbeKey = (String, Vec<String>);

thread_local! {
    /// Probe results by module name and search path, so a module `use`d from
    /// several files of one process runs at parse time once.
    static PROBE_CACHE: RefCell<HashMap<ProbeKey, Rc<Vec<InlineModuleExport>>>> =
        RefCell::new(HashMap::new());
}

/// Run `module` at parse time and return the routines its load exported, as
/// parser export records. A probe that fails (the module dies while loading)
/// contributes nothing: the program's own run-time `use` loads the module
/// again and reports the failure there, at its real location.
///
/// A non-executing parse (`crate::parser::no_execute`) never runs the module:
/// it contributes nothing, leaving the static scan's exports in place, and is
/// not cached so a later executing parse still probes.
pub(super) fn probe_dynamic_exports(module: &str) -> Rc<Vec<InlineModuleExport>> {
    if crate::parser::no_execute::no_execute() {
        return Rc::new(Vec::new());
    }
    let lib_paths = super::parser_lib_paths();
    let key = (module.to_string(), lib_paths.clone());
    if let Some(hit) = PROBE_CACHE.with(|c| c.borrow().get(&key).cloned()) {
        return hit;
    }
    let names =
        crate::runtime::parse_time_exports::probe_module_exports(module.to_string(), lib_paths)
            .unwrap_or_default();
    let exports: Rc<Vec<InlineModuleExport>> = Rc::new(
        names
            .into_iter()
            .map(|export| {
                let (precedence, associativity) = parser_precedence(export.op_prec.as_ref());
                InlineModuleExport {
                    name: export.name,
                    precedence,
                    associativity,
                    is_test_assertion: false,
                }
            })
            .collect(),
    );
    PROBE_CACHE.with(|c| c.borrow_mut().insert(key, exports.clone()));
    exports
}

/// The parser's numeric precedence level and associativity for a probed
/// operator's `Routine.prec` hash: the level of the built-in operator it was
/// derived from, moved by five per `is tighter` / `is looser` step.
// Cost: O(n), n = built-in operator count.
fn parser_precedence(prec: Option<&crate::op_prec::OpPrec>) -> (Option<i32>, Option<String>) {
    let Some(prec) = prec else {
        return (None, None);
    };
    let field = |key: &str| {
        prec.entries()
            .find(|(k, _)| *k == key)
            .map(|(_, v)| v.to_string())
    };
    let level = field("prec").and_then(|level| {
        let (base, steps) = crate::op_prec::split_prec_marks(&level);
        crate::op_prec::infix_symbols_with_prec(&base)
            .find_map(|symbol| super::super::registry::resolve_op_precedence(symbol))
            .map(|anchor| anchor + 5 * steps)
    });
    let assoc = field("assoc").filter(|assoc| assoc != "left");
    (level, assoc)
}
