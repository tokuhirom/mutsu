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
//! Bucketed kinds: Int/BigInt, Str, Bool, Num, Rat, FatRat, Complex, Pair, Array,
//! Hash, Date/DateTime and plain objects (by id, or by the `WHICH` a class
//! overrides it with). Unbucketed kinds (Seq, Slip, Enum, Set/Bag/Mix, routines,
//! Version, ObjAt, ...) keep the scan.
//!
//! That is why a kind with a cross-kind identity arm gets no bucket at all.
//! `Package("int")` is `values_identical` to `Int(0)`, and a `Mixin` carrying
//! only the read-only topic marker is transparent to its inner value, so both
//! stay unbucketed — and every unbucketed key is scanned by *every* candidate,
//! whether that candidate is bucketed or not. `Int` and `BigInt` do have a
//! cross-kind arm (they compare numerically), so they share one bucket space
//! rather than being excluded.

use crate::value::identity::values_identical;
use crate::value::types::is_stash_class_name;
use crate::value::{Value, ValueView};
use num_traits::ToPrimitive;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// The bucket of a `Str` value; also a `Pair`'s key, which `eqv` treats as the
/// `Str` it is.
fn str_bucket(s: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    2u8.hash(&mut hasher);
    s.hash(&mut hasher);
    hasher.finish()
}

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
        ValueView::Str(s) => return Some(str_bucket(s.as_str())),
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
        // `===` on these falls through to `eqv`, which is structural within one
        // variant and false across variants (`1/2 eqv 0.5` is False), so the
        // variant tag plus the payload is a bucket two identical values share.
        ValueView::Rat(n, d) => {
            5u8.hash(&mut hasher);
            n.hash(&mut hasher);
            d.hash(&mut hasher);
        }
        ValueView::FatRat(n, d) => {
            6u8.hash(&mut hasher);
            n.hash(&mut hasher);
            d.hash(&mut hasher);
        }
        // Bit-exact per component, every NaN one class: the `eqv` rule `Num` uses.
        ValueView::Complex(re, im) => {
            7u8.hash(&mut hasher);
            for part in [re, im] {
                if part.is_nan() {
                    u64::MAX.hash(&mut hasher);
                } else {
                    part.to_bits().hash(&mut hasher);
                }
            }
        }
        // `Pair` and `ValuePair` are one equality class (`eqv` compares a Str
        // key of either flavour), so a Str key hashes the same for both; any
        // other key must itself have a bucket. The value is not hashed: it is
        // compared by the predicate.
        ValueView::Pair(key, _) => {
            8u8.hash(&mut hasher);
            str_bucket(key.as_str()).hash(&mut hasher);
        }
        // A key `eqv` compares through its own rules (an object key by
        // attributes, not id) gets no bucket: only the leaf scalars whose
        // hash above is exactly their `eqv` class are admitted.
        ValueView::ValuePair(key, _) => {
            if !matches!(
                key.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Str(_)
                    | ValueView::Bool(_)
                    | ValueView::Num(_)
            ) {
                return None;
            }
            8u8.hash(&mut hasher);
            identity_bucket(key)?.hash(&mut hasher);
        }
        // Containers are `===` only when they are the same node. A `Seq` that
        // reads as a List (`.cache`) can still be `eqv` to an Array, but a
        // `Seq` has no bucket here and so is scanned against every candidate.
        ValueView::Array(items, _) => {
            11u8.hash(&mut hasher);
            (crate::gc::Gc::as_ptr(&items) as usize).hash(&mut hasher);
        }
        ValueView::Hash(items) => {
            12u8.hash(&mut hasher);
            (crate::gc::Gc::as_ptr(&items) as usize).hash(&mut hasher);
        }
        ValueView::Instance {
            class_name, id, ..
        } => {
            // The classes `values_identical` decides by content or through
            // another rule than the instance id keep no bucket.
            let name = class_name.as_str();
            if is_stash_class_name(name)
                || matches!(name, "Supply" | "IO::Special" | "ObjAt" | "ValueObjAt" | "IO::Handle")
                || (name.starts_with("Perl6::Metamodel::") && name.ends_with("HOW"))
            {
                return None;
            }
            if let ValueView::Instance { attributes, .. } = value.view()
                && attributes.contains_key("__mutsu_version_value")
            {
                return None;
            }
            match value.user_which_memo() {
                // A class that overrides `WHICH` is decided by that answer:
                // two objects with the same one are `===`.
                Some(which) => {
                    9u8.hash(&mut hasher);
                    which.hash(&mut hasher);
                }
                // Date and DateTime are value types: identical when their
                // calendar value is, whichever object holds it.
                None if matches!(name, "Date" | "DateTime") => {
                    13u8.hash(&mut hasher);
                    crate::value::which_key::value_which_key(value).hash(&mut hasher);
                }
                None => {
                    10u8.hash(&mut hasher);
                    id.hash(&mut hasher);
                }
            }
        }
        _ => return None,
    }
    Some(hasher.finish())
}

/// The values `unique` / `repeated` have already seen, with a hash index over
/// the kinds that admit one.
#[derive(Debug, Clone)]
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
