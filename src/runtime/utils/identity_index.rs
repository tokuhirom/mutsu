//! Average-O(1) duplicate lookup for `unique` and `repeated`.
//!
//! Both methods compare every candidate against every value already seen,
//! which made them O(n^2): `(^160_000).unique` never finished under mutsu
//! while rakudo answered in 0.5s. It was found through `MongoDB::Fast`'s
//! `t/08-request-id-unique.rakutest`, which dedupes 160_000 concurrently
//! minted request ids. Rakudo keys its `unique` on a `.WHICH` hash; this
//! module is the same idea expressed against mutsu's identity predicate.
//!
//! The index is deliberately a *bucketing* structure, not a replacement for
//! the identity test. [`identity_bucket`] hands out a hash only for the value
//! kinds whose `values_identical` equality class is decided by cheap, hashable
//! content, and the lookups still run the full predicate on every candidate a
//! bucket produces. So a hash collision costs a comparison and never a wrong
//! answer, and the only invariant the bucket function must uphold is the
//! weaker one:
//!
//! > two values that compare identical must never land in *different* buckets.
//!
//! That is why a kind with a cross-kind identity arm gets no bucket at all.
//! `Package("int")` is `values_identical` to `Int(0)`, and a `Mixin` carrying
//! only the read-only topic marker is transparent to its inner value, so both
//! stay unbucketed — and every unbucketed key is scanned by *every* candidate,
//! whether that candidate is bucketed or not. `Int` and `BigInt` do have a
//! cross-kind arm (they compare numerically), so they share one bucket space
//! rather than being excluded.

use super::shaped::values_identical;
use crate::value::{Value, ValueView};
use num_traits::ToPrimitive;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// A hash that two identical values are guaranteed to share, or `None` when
/// this value kind has no such hash (see the module comment).
fn identity_bucket(value: &Value) -> Option<u64> {
    let mut hasher = DefaultHasher::new();
    match value.view() {
        // Int and BigInt compare numerically across the two representations,
        // so a BigInt that fits in an i64 must hash like the Int it equals.
        ValueView::Int(i) => {
            0u8.hash(&mut hasher);
            i.hash(&mut hasher);
        }
        ValueView::BigInt(b) => match b.to_i64() {
            Some(i) => {
                0u8.hash(&mut hasher);
                i.hash(&mut hasher);
            }
            None => {
                1u8.hash(&mut hasher);
                b.to_string().hash(&mut hasher);
            }
        },
        ValueView::Str(s) => {
            2u8.hash(&mut hasher);
            s.as_str().hash(&mut hasher);
        }
        ValueView::Bool(b) => {
            3u8.hash(&mut hasher);
            b.hash(&mut hasher);
        }
        // All NaN bit patterns are one equality class for `eqv`, so they must
        // be one bucket; every other Num compares bit-exactly (which keeps
        // `0e0` and `-0e0` apart, as `eqv` does).
        ValueView::Num(n) => {
            4u8.hash(&mut hasher);
            if n.is_nan() {
                u64::MAX.hash(&mut hasher);
            } else {
                n.to_bits().hash(&mut hasher);
            }
        }
        _ => return None,
    }
    Some(hasher.finish())
}

/// The values `unique` / `repeated` have already seen, with a hash index over
/// the kinds that admit one.
pub(crate) struct IdentityIndex {
    seen: Vec<Value>,
    /// bucket hash -> indices into `seen`
    buckets: HashMap<u64, Vec<usize>>,
    /// indices into `seen` of every value with no bucket; scanned by *every*
    /// candidate, since an unbucketed value may be identical to a bucketed one.
    unbucketed: Vec<usize>,
}

impl IdentityIndex {
    pub(crate) fn new() -> Self {
        Self {
            seen: Vec::new(),
            buckets: HashMap::new(),
            unbucketed: Vec::new(),
        }
    }

    /// True when `candidate` matches an already-inserted value under `same`.
    ///
    /// `same` is passed in because the two `unique` implementations disagree
    /// on one edge: the runtime dispatch path treats two placeholder-id
    /// instances of the same class as distinct, the native fast path does not.
    /// Narrowing the scan must not quietly pick one of those for the other.
    pub(crate) fn contains_by(
        &self,
        candidate: &Value,
        same: impl Fn(&Value, &Value) -> bool,
    ) -> bool {
        match identity_bucket(candidate) {
            // A bucketed candidate can only match a value in its own bucket,
            // or an unbucketed one.
            Some(hash) => {
                let bucket = self.buckets.get(&hash).map(Vec::as_slice).unwrap_or(&[]);
                bucket
                    .iter()
                    .chain(self.unbucketed.iter())
                    .any(|&i| same(&self.seen[i], candidate))
            }
            // An unbucketed candidate has no equality class we can narrow, so
            // it falls back to the full scan.
            None => self.seen.iter().any(|seen| same(seen, candidate)),
        }
    }

    /// [`IdentityIndex::contains_by`] with the plain `===` identity predicate.
    pub(crate) fn contains(&self, candidate: &Value) -> bool {
        self.contains_by(candidate, values_identical)
    }

    pub(crate) fn insert(&mut self, value: Value) {
        let index = self.seen.len();
        match identity_bucket(&value) {
            Some(hash) => self.buckets.entry(hash).or_default().push(index),
            None => self.unbucketed.push(index),
        }
        self.seen.push(value);
    }
}
