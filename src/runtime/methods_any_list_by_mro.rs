//! `Any`'s iteration methods answered by their rows, by the receiver's MRO
//! (ADR-11276 §9.134).
//!
//! `map`, `grep`, `first`, `reduce`, ... are `Handler::Interp` rows of `Any`
//! (`method_table::collections::any_interp`) whose handlers call the one
//! `Interpreter::dispatch_*_method` each. The by-name cascades used to keep an
//! arm per name calling that same routine, for the calls the table entry in
//! `call_method_with_values` does not reach (an instance, a type object, a
//! call whose arguments a row does not bind). Those calls now walk the
//! receiver's MRO for the first row that binds, as `.^can` would find it.

use super::*;

/// The methods whose `Any` rows answer by the receiver's MRO.
const ANY_LIST_BY_MRO: [&str; 72] = [
    "are",
    "nl-out",
    "chomp",
    "pending",
    "say",
    "print",
    "put",
    "note",
    "printf",
    "sprintf",
    "shape",
    "return-rw",
    "polymod",
    "can",
    "does",
    "list",
    "Array",
    "elems",
    "Setty",
    "Baggy",
    "Mixy",
    "toggle",
    "tree",
    "is-lazy",
    "rotate",
    "join",
    "Seq",
    "List",
    "Set",
    "SetHash",
    "Bag",
    "BagHash",
    "Mix",
    "MixHash",
    "Map",
    "Hash",
    "hash",
    "slice",
    "take",
    "new-from-pairs",
    "any",
    "all",
    "one",
    "none",
    "min",
    "max",
    "minmax",
    "minpairs",
    "maxpairs",
    "sort",
    "unique",
    "repeated",
    "map",
    "flatmap",
    "deepmap",
    "duckmap",
    "nodemap",
    "grep",
    "first",
    "reduce",
    "produce",
    "iterator",
    "eager",
    "squish",
    "rotor",
    "skip",
    "collate",
    "classify",
    "categorize",
    "classify-list",
    "categorize-list",
    // `Any.reverse` (`self.list.reverse`) of an instance is its twin's (#13819).
    "reverse",
];

impl Interpreter {
    /// Answer one of `Any`'s iteration methods from the first row of the
    /// receiver's MRO that binds the call, or `None` for any other method and
    /// for a `Junction`, whose methods autothread.
    // Cost: O(n) to test the name, n = 15, plus one memoized MRO read and
    // `invoke_mro`'s own cost.
    pub(super) fn any_list_method_by_mro(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // A junction's methods autothread elsewhere, except the ones the
        // junction answers itself: `Mu`'s (`does`, `can`, the printing
        // routines, which write the junction's own gist or `Str`) and `map`
        // (ADR-11276 §9.151).
        if !ANY_LIST_BY_MRO.contains(&method)
            || (matches!(target.view(), ValueView::Junction { .. })
                && Self::junction_invocant_autothreads(method))
        {
            return None;
        }
        // An `is Pair` or quant-hash subclass answers its base's methods on
        // the value it carries (`native_base::redirect`), not `Any`'s on itself.
        if crate::builtins::native_base::has_collection_payload(target)
            && crate::builtins::native_base::redirect(target, method).is_some()
        {
            return None;
        }
        // `Mu.say`/`print`/`put`/`note` take no argument; a call with some is a
        // class acting as `$*OUT` (its own `print`), answered by the cascade.
        if matches!(method, "say" | "print" | "put" | "note") && !args.is_empty() {
            return None;
        }
        // `.^can($name)` / `.^does($type)` is the HOW's two-argument call
        // (`$obj.HOW.can($obj, $name)`), answered by the metaobject.
        if matches!(method, "can" | "does") && args.len() != 1 {
            return None;
        }
        // The list coercions of every receiver but a `Supply` keep the
        // answers of the native paths and of the cascades after this walk;
        // a `Supply` materializes through its own `list` (§9.136).
        if matches!(method, "list" | "Array")
            && !matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
        {
            return None;
        }
        let mro = self.classhow_mro_unhidden_names(target);
        let owners: Vec<&str> = mro.iter().map(String::as_str).collect();
        crate::builtins::method_table::invoke_mro(self, &owners, method, args, target)
    }

    /// `Any.flatmap`: `.map(...)` with each mapper result flattened one level.
    // Cost: O(e + r), e = invocant elements (one mapper call each), r =
    // elements the mapper results flatten to (a returned gather is run to
    // completion here).
    pub(crate) fn dispatch_flatmap_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let mapped = self.dispatch_map_method(target, args)?;
            // ADR-0058: `.map` now hands back a Seq whose callback has
            // not run; the flattening below reads its elements through
            // pure code, so pull it here.
            self.reify_map_grep_seq(&mapped)?;
            let items = Self::value_to_list(&mapped);
            let mut flat_items = Vec::new();
            for item in items {
                self.reify_map_grep_seq(&item)?;
                self.reify_gather_seq(&item)?;
                match item.view() {
                    // `.flatmap` is `.map(...).flat`: a mapper result flattens
                    // all the way down, itemized sublists aside.
                    ValueView::Array(..) | ValueView::Seq(..) => {
                        crate::builtins::flat_val(&item, &mut flat_items, true);
                    }
                    // A bare Hash flattens to its pairs; an itemized one (`$h`,
                    // an element of a `[...]`) stays single.
                    ValueView::Hash(_) if !item.hash_is_itemized() => {
                        crate::builtins::flat_val(&item, &mut flat_items, true);
                    }
                    // Ranges are iterable values too. A mapper can
                    // return a Range (for example, `* * 2` applied to
                    // a Range), and `.flatmap` must flatten that
                    // result just like `.flat` does.
                    ValueView::Range(..)
                    | ValueView::RangeExcl(..)
                    | ValueView::RangeExclStart(..)
                    | ValueView::RangeExclBoth(..)
                    | ValueView::GenericRange { .. } => {
                        flat_items.extend(crate::runtime::utils::value_to_list(&item));
                    }
                    // A mapper returning a `gather` (Getopt::Long's
                    // `make-receivers` for a Bool option) hands back an
                    // unforced coroutine; flattening means iterating it,
                    // which only the interpreter can do. A `lazy`-marked
                    // or infinite list stays a single element rather
                    // than hanging the eager flatten.
                    ValueView::LazyList(ll) if !ll.is_genuinely_lazy() => {
                        flat_items.extend(self.force_lazy_list_vm(&ll)?);
                    }
                    _ => flat_items.push(item.clone()),
                }
            }
            Ok(Value::seq(flat_items))
    }
}
