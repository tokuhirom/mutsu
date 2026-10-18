//! The autovivification of an undefined binding by `Any`'s growing and keyed
//! methods (ADR-11276 §9.171, §9.172).
//!
//! `my $x; $x.push(1)` turns the container into an `Array`, and
//! `$x.ASSIGN-KEY('a', 1)` into a `Hash`: Rakudo's `Any:U \SELF` candidates
//! assign a fresh container to `SELF` and act on it. The `Any` rows
//! (`method_table::mutating::autoviv`) call this for a receiver that names a
//! binding, then act on the container through `Array`'s or `Hash`'s row.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Store `fresh`, an empty `Array` or `Hash`, in the binding `name` and
    /// answer it as stored. A `$` variable holds it in its `Scalar` container
    /// (`my $x; $x.push(1); $x.raku` is `$[1]`), and so does a never-written
    /// package-qualified `@`/`%` slot, which reads as `Any`
    /// (`@GLOBAL::a.push(1)` is `$[1]`, #10962); an `@`, `%` or `&` lexical
    /// holds it bare.
    // Cost: O(1), one env write (a `:=` alias is descended), plus one `our`
    // write for a package-qualified name.
    pub(crate) fn vivify_binding(&mut self, name: &str, fresh: Value) -> Value {
        let sym = Symbol::intern(name);
        let package_slot =
            crate::qualified::is_package_array(sym) || crate::qualified::is_package_hash(sym);
        let vivified = if name.starts_with(['@', '%', '&']) && !package_slot {
            fresh
        } else {
            fresh.item()
        };
        // A writable loop parameter such as `for @rows <-> $row` names a
        // `ContainerRef` cell for the source element. Replacing the env
        // binding here detaches the vivified array from that element, so later
        // reads still see Any. Descend through the alias when one exists;
        // ordinary lexical variables retain the old env fallback.
        if let Some(slot) = self.env_root_descended_mut(name) {
            *slot = vivified.clone();
        } else {
            self.env_mut().insert(name.to_string(), vivified.clone());
            // A package-qualified name (`$GLOBAL::n.push(1)`) is a package
            // variable. The env write reaches only the running frame, whose
            // exit drops a key it did not hold on entry, so persist the
            // vivified array in `our_vars` as `SetGlobal` and the
            // read-modify-write store do (#10620). The two share one array, so
            // the mutation that follows lands in both.
            if crate::qualified::is_qualified(sym) {
                self.set_our_var(name.to_string(), vivified.clone());
            }
        }
        vivified
    }
}
