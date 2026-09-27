//! How the base-name key index (`Interpreter::fn_keys_by_base`, read through
//! [`Interpreter::fn_keys_for_base`]) fills a base name it has no entry for.
//!
//! The index started out filled lazily per base name: each miss scanned the
//! whole functions map for that one name, and a registration evicted its own
//! base name, so the next lookup of that name scanned again. That is O(r) per
//! freshly-registered name looked up, r = registered functions — which made a
//! stash `&` binding re-aliasing a multi O(r), and a `BEGIN for <&a &b ...> {
//! EXPORT::DEFAULT::{$_} = ::($_) }` re-export loop O(n·r)
//! ([#9665](https://github.com/tokuhirom/mutsu/issues/9665)).
//!
//! Now a miss can be answered for many base names by ONE scan:
//!
//! - Once the index is **complete** (an entry for every base name with a
//!   registered key, built in one pass), a miss on a base name nobody evicted
//!   means that name has no keys at all — O(1).
//! - An eviction on a complete index marks its base name **dirty**, and a miss
//!   on a dirty base name refills every dirty base name in the same single
//!   pass. Registering n names and then looking each up costs one scan, not n.
//!
//! The eviction contract callers rely on is unchanged: naming any key evicts
//! that key's whole base name (several sites name one representative key for a
//! family they rewrote). Only the refill got cheaper.
//!
//! A clear (`invalidate_fn_resolution`) drops completeness. The first miss after
//! it is still a single-name scan, exactly as before; only a second miss before
//! the next clear builds the complete index. So a program that clears the index
//! and then looks up one name, over and over, pays what it always paid, and one
//! that looks up two or more names pays for one full pass instead of one scan
//! per name.

use super::dispatch_resolve::function_key_base_name;
use super::*;

/// Completeness bookkeeping for `Interpreter::fn_keys_by_base`.
#[derive(Default)]
pub(crate) struct FnKeysIndexState {
    /// Every base name with a registered key has an entry, except the `dirty`
    /// ones; a base name with neither has no keys.
    complete: bool,
    /// Misses answered by a single-name scan since the last clear, while
    /// incomplete. The second one builds the complete index instead.
    cold_misses: u32,
    /// Base names evicted since the index became complete: entries unknown.
    /// Spelled as slices of interned keys, so they are `'static`.
    dirty: rustc_hash::FxHashSet<&'static str>,
}

impl Interpreter {
    /// Drop `key`'s base-name entry from the index; on a complete index, mark
    /// the base name dirty so its next lookup refills it.
    // Cost: O(m), m = key bytes (base-name reduction, intern, two hash probes).
    pub(crate) fn evict_fn_keys_base(&mut self, key: Symbol) -> bool {
        let base = function_key_base_name(key.as_str());
        let evicted = self.fn_keys_by_base.remove(&Symbol::intern(base)).is_some();
        if self.fn_keys_index.complete {
            self.fn_keys_index.dirty.insert(base);
        }
        evicted
    }

    /// Drop the whole index, completeness included.
    // Cost: O(b), b = indexed base names.
    pub(crate) fn clear_fn_keys_index(&mut self) {
        self.fn_keys_by_base.clear();
        self.fn_keys_index = FnKeysIndexState::default();
    }

    /// Answer a miss on `base` (not in `fn_keys_by_base`) and index the answer.
    // Cost: O(1) on a complete index for a base name nobody evicted; otherwise
    // one pass over the functions map, O(r), r = registered functions, which
    // also refills every other dirty base name.
    pub(crate) fn fill_fn_keys_base(&mut self, base: &str, base_sym: Symbol) -> Arc<[Symbol]> {
        if !self.fn_keys_index.complete {
            self.fn_keys_index.cold_misses += 1;
            if self.fn_keys_index.cold_misses < 2 {
                let keys = self.collect_fn_keys_for_base(base);
                self.fn_keys_by_base.insert(base_sym, keys.clone());
                return keys;
            }
            self.build_fn_keys_index(None);
        } else if self.fn_keys_index.dirty.contains(base) {
            let dirty = std::mem::take(&mut self.fn_keys_index.dirty);
            if dirty.len() == 1 {
                // The per-call `my sub` re-install cycle (#8314) dirties one
                // name at a time: a plain comparing scan, as cheap as before.
                let keys = self.collect_fn_keys_for_base(base);
                self.fn_keys_by_base.insert(base_sym, keys.clone());
                return keys;
            }
            self.build_fn_keys_index(Some(&dirty));
        }
        // Complete and `base` is clean now: an entry, or no keys at all.
        self.fn_keys_by_base
            .entry(base_sym)
            .or_insert_with(|| Arc::from([]))
            .clone()
    }

    /// One pass over the functions map, (re)indexing every base name in `only`
    /// — or every base name, when `only` is `None` — and marking the index
    /// complete. A base name in `only` with no keys gets an empty entry.
    // Cost: O(r), r = registered functions.
    fn build_fn_keys_index(&mut self, only: Option<&rustc_hash::FxHashSet<&'static str>>) {
        let mut by_base: rustc_hash::FxHashMap<&'static str, Vec<Symbol>> = Default::default();
        if let Some(only) = only {
            for base in only {
                by_base.insert(base, Vec::new());
            }
        }
        {
            let registry = self.registry();
            crate::vm::vm_stats::record_fn_keys_base_scan(registry.functions.len());
            for key in registry.functions.keys() {
                let base = function_key_base_name(key.as_str());
                match only {
                    Some(_) => {
                        if let Some(keys) = by_base.get_mut(base) {
                            keys.push(*key);
                        }
                    }
                    None => by_base.entry(base).or_default().push(*key),
                }
            }
        }
        for (base, keys) in by_base {
            self.fn_keys_by_base
                .insert(Symbol::intern(base), Arc::from(keys));
        }
        self.fn_keys_index.complete = true;
        self.fn_keys_index.dirty.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn indexed(i: &Interpreter, base: &str) -> bool {
        i.fn_keys_by_base.contains_key(&Symbol::intern(base))
    }

    /// The first miss after a clear is a single-name scan; the second builds
    /// the complete index, after which an unregistered name is answered
    /// without a scan and evicted names refill together.
    #[test]
    fn evicted_base_names_refill_in_one_pass() {
        let mut i = Interpreter::new();
        i.run("sub alpha() { 1 }\nsub beta() { 2 }\nsub gamma() { 3 }\n")
            .expect("setup program runs");
        i.invalidate_fn_resolution();

        // First miss: that name only, index still incomplete.
        assert!(!i.fn_keys_for_base("alpha").is_empty());
        assert!(!indexed(&i, "beta"));
        assert!(!i.fn_keys_index.complete);

        // Second miss: the complete index, every base name at once.
        assert!(!i.fn_keys_for_base("beta").is_empty());
        assert!(i.fn_keys_index.complete);
        assert!(indexed(&i, "gamma"));
        assert!(i.fn_keys_for_base("never-declared").is_empty());

        // Evicting two names marks both dirty; looking up one refills both.
        let gamma = i.fn_keys_for_base("gamma");
        i.invalidate_fn_resolution_for_keys([gamma[0]]);
        let alpha = i.fn_keys_for_base("alpha");
        i.invalidate_fn_resolution_for_keys([alpha[0]]);
        assert!(!indexed(&i, "alpha") && !indexed(&i, "gamma"));
        assert_eq!(&*i.fn_keys_for_base("alpha"), &*alpha);
        assert!(
            indexed(&i, "gamma"),
            "the other dirty name refilled in the same pass"
        );
        assert_eq!(&*i.fn_keys_for_base("gamma"), &*gamma);
        assert!(i.fn_keys_index.dirty.is_empty());
    }

    /// A name registered after the index became complete is found: its
    /// registration evicts (dirties) its base name, so the complete index
    /// does not answer "no keys" for it.
    #[test]
    fn a_name_registered_after_completion_is_found() {
        let mut i = Interpreter::new();
        i.run("sub alpha() { 1 }\nsub beta() { 2 }\n")
            .expect("setup program runs");
        i.invalidate_fn_resolution();
        i.fn_keys_for_base("alpha");
        i.fn_keys_for_base("beta");
        assert!(i.fn_keys_index.complete);

        i.run("sub delta() { 4 }\n").expect("second program runs");
        let fresh = i.collect_fn_keys_for_base("delta");
        assert!(!fresh.is_empty(), "delta is registered");
        assert_eq!(&*i.fn_keys_for_base("delta"), &*fresh);
    }
}
