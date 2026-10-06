//! Listing a multi family's candidate keys without scanning the registry.
//!
//! A multi family `Pkg::name` keeps its candidates under keys spelled
//! `Pkg::name/<suffix>`. Asking "which keys of the function map belong to this
//! family?" used to be a scan of every key in the map, which made each export,
//! import and hoist bookkeeping step O(registry) -- and a module load does one
//! per exported name (#11756). The interned-name family index
//! ([`crate::qualified_tail_index::names_in_family`]) is a superset of those
//! keys for a qualified family, so probing just its names gives the same
//! answer.

use super::function_table::FunctionTable;
use crate::ast::FunctionDef;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use std::sync::Arc;

impl FunctionTable {
    /// The keys of this table spelled `family/…`: the candidate keys of the
    /// multi family `family`, in no particular order.
    ///
    /// A qualified, sigil-less family is answered from the interned-name
    /// family index, which lists every interned qualified name spelled that
    /// way; anything else falls back to a scan.
    // Cost: O(k), k = interned names spelled `family/…`, for a qualified
    // family (amortized, see `names_in_family`); O(n), n = keys in the table,
    // otherwise.
    pub(crate) fn family_keys(&self, family: &str) -> Vec<Symbol> {
        let in_family = |key: &Symbol| {
            key.as_str()
                .strip_prefix(family)
                .is_some_and(|rest| rest.starts_with('/'))
        };
        if crate::qualified::is_qualified_str(family) && !family.starts_with(['$', '@', '%', '&']) {
            let mut keys = crate::qualified_tail_index::names_in_family(family);
            keys.retain(|key| in_family(key) && self.contains_key(key));
            keys
        } else {
            self.keys().filter(|key| in_family(key)).copied().collect()
        }
    }
}

impl Interpreter {
    /// Push every candidate of `family` (its keys `family_keys`, as listed by
    /// [`FunctionTable::family_keys`]) onto `out`, re-keyed under `target`:
    /// `family/2:Int` becomes `target/2:Int`.
    // Cost: O(k), k = `family_keys.len()`.
    pub(crate) fn rebase_family_entries(
        &self,
        family: &str,
        family_keys: &[Symbol],
        target: &str,
        out: &mut Vec<(Symbol, Arc<FunctionDef>)>,
    ) {
        let functions = &self.registry().functions;
        for key in family_keys {
            let (Some(suffix), Some(def)) =
                (key.as_str().strip_prefix(family), functions.get(key))
            else {
                continue;
            };
            out.push((Symbol::intern(&format!("{target}{suffix}")), def.clone()));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::function_table::tests::def;

    fn table(keys: &[&str]) -> FunctionTable {
        let mut t = FunctionTable::default();
        for k in keys {
            t.map_mut()
                .insert(Symbol::intern(k), def());
        }
        t
    }

    fn sorted(mut v: Vec<Symbol>) -> Vec<String> {
        let mut s: Vec<String> = v.drain(..).map(|k| k.as_str().to_string()).collect();
        s.sort();
        s
    }

    #[test]
    fn qualified_family_matches_a_scan() {
        // An interned but unregistered name, and a longer name sharing the
        // family's text as a prefix, are both excluded.
        Symbol::intern("FamT1::f/9");
        let t = table(&["FamT1::f", "FamT1::f/1", "FamT1::f/2:Int", "FamT1::ff/1", "FamT1::g/1"]);
        assert_eq!(
            sorted(t.family_keys("FamT1::f")),
            vec!["FamT1::f/1".to_string(), "FamT1::f/2:Int".to_string()]
        );
        assert!(t.family_keys("FamT1::h").is_empty());
    }

    #[test]
    fn unqualified_family_scans() {
        let t = table(&["famt2/1", "famt2/2", "famt2x/1"]);
        assert_eq!(
            sorted(t.family_keys("famt2")),
            vec!["famt2/1".to_string(), "famt2/2".to_string()]
        );
    }
}
