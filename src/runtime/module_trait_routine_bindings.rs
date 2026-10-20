//! A module's trait-composed routines stay out of the importer's env.
//!
//! A routine trait that composes a role (`$r does R`, which upstream
//! NativeCall's `is native` does) writes the composed routine back under the
//! routine's `&name` env key (`register_sub_decl`). A module body runs in its
//! loader's env, so that key outlived the load and the importer's `&name`
//! named the module's routine: `use Compress::Zlib` made `.&compress` call
//! `Compress::Zlib::Raw`'s native `compress`, which Compress::Zlib only
//! `need`s, instead of the `compress` it exports (#12907). In rakudo the
//! routine's `&name` is a lexical of the module's compunit.
//!
//! The composition itself lives in the routine's identity cell (ADR-11827),
//! which every code object of the routine shares, so the env key carries
//! nothing a later reader needs: dropping it leaves the module's own reads
//! and the importer's imports resolving through the routine registry.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Remove the `&name` env bindings a module load created for its own
    /// trait-composed routines: a `&name` key absent before the load, whose
    /// value is a routine named `name` with roles composed into its identity
    /// cell.
    // Cost: O(e), e = env keys (one set probe each; the value test is O(1)).
    pub(super) fn drop_module_trait_routine_bindings(
        &mut self,
        before_env_keys: &std::collections::HashSet<Symbol>,
    ) {
        let stale: Vec<Symbol> = self
            .env
            .keys()
            .copied()
            .filter(|key| !before_env_keys.contains(key))
            .filter(|key| {
                let name = key.resolve();
                let Some(bare) = name.strip_prefix('&') else {
                    return false;
                };
                self.env
                    .get_sym(*key)
                    .is_some_and(|value| Self::is_composed_routine_named(value, bare))
            })
            .collect();
        for key in stale {
            self.env.remove_sym(key);
        }
    }

    /// Whether `value` is the routine `name` itself, seen with the roles a
    /// trait composed into it: a `Mixin` over a `Sub` of that name whose
    /// identity cell records the composition.
    // Cost: O(1).
    fn is_composed_routine_named(value: &Value, name: &str) -> bool {
        let ValueView::Mixin(inner, _) = value.view() else {
            return false;
        };
        let ValueView::Sub(data) = inner.view() else {
            return false;
        };
        data.name.resolve() == name && data.routine_cell.get().is_some()
    }
}
