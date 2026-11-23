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
//! Bucketed kinds: Int/BigInt, Str, Bool, Num, Rat, FatRat, big Rats,
//! Complex, Pair, Array, Hash, Seq, Slip, LazyList, Enum, Set/Bag/Mix,
//! Version, Subs, regexes, type objects, Nil, Whatever, integer Ranges, mixins
//! (allomorphs) over a bucketed base, RakuAST nodes, Date/DateTime, ObjAt,
//! metaobjects and plain objects (by id, or by the `WHICH` a class overrides
//! it with). A container is bucketed as the value it holds.
//!
//! A kind with a cross-kind identity arm either shares the other kind's bucket
//! or gets none. `Int` and `BigInt` compare numerically, so they share one
//! bucket space. `Package("int")` is `values_identical` to `Int(0)` and
//! `Package("Nil")` to `Nil`, so each takes that value's bucket. The rest stay
//! unbucketed, and every unbucketed value is scanned by *every* candidate,
//! bucketed or not:
//! - a Junction, whose `eqv` threads, so it can be `===` to any kind;
//! - a mixed type object, which is also its composed class's type object;
//! - a by-name `Routine` handle, which a runtime hook compares;
//! - a `.cache` List view of a Seq, which is `eqv` to a List;
//! - Capture, Proxy, HyperSeq/RaceSeq, Promise/Channel, Buf and parameterized
//!   roles, which have no bucket yet.

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
        // By `.WHICH` id, which a read snapshot of the node shares with it
        // (`values_identical` accepts either the same node or the same id).
        ValueView::Array(items, _) => {
            11u8.hash(&mut hasher);
            items.which_id.get().hash(&mut hasher);
        }
        ValueView::Hash(items) => {
            12u8.hash(&mut hasher);
            items.which_id.get().hash(&mut hasher);
        }
        ValueView::Instance { class_name, id, .. } => {
            // The classes `values_identical` decides by content or through
            // another rule than the instance id keep no bucket.
            let name = class_name.as_str();
            if is_stash_class_name(name)
                || crate::value::supply_class::is_supply_class(class_name)
                || matches!(name, "IO::Special" | "IO::Handle")
            {
                return None;
            }
            let memo = value.user_which_memo();
            if memo.is_none()
                && let ValueView::Instance { attributes, .. } = value.view()
            {
                // An ObjAt is `===` by its identity text, a metaobject by the
                // type it introspects, a Version subclass by its value.
                if matches!(name, "ObjAt" | "ValueObjAt") {
                    (29u8, name, attributes.as_map().objat_which()).hash(&mut hasher);
                    return Some(hasher.finish());
                }
                if name.starts_with("Perl6::Metamodel::") && name.ends_with("HOW") {
                    let introspected = attributes.as_map().get("name").map(|v| v.to_string_value());
                    (30u8, name, introspected).hash(&mut hasher);
                    return Some(hasher.finish());
                }
                if attributes.contains_key("__mutsu_version_value") {
                    (31u8, name).hash(&mut hasher);
                    crate::value::which_key::value_which_key(value).hash(&mut hasher);
                    return Some(hasher.finish());
                }
            }
            match memo {
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
        // `===` looks through a container to the value it holds.
        ValueView::Scalar(_) | ValueView::ContainerRef(_) => {
            return identity_bucket(&value.descalarize().deref_container());
        }
        // A `Seq` is `===` only to itself. A `.cache` List view of one is
        // `eqv` to a List, so it keeps the scan.
        ValueView::Seq(body)
            if !matches!(
                body.view(),
                crate::value::SeqView::List | crate::value::SeqView::ItemList
            ) =>
        {
            14u8.hash(&mut hasher);
            body.identity().hash(&mut hasher);
        }
        // Every empty Slip is the one `Empty`; any other is `===` only to itself.
        ValueView::Slip(items) => {
            15u8.hash(&mut hasher);
            if !items.is_empty() {
                (std::sync::Arc::as_ptr(&items) as usize).hash(&mut hasher);
            }
        }
        ValueView::LazyList(list) => {
            16u8.hash(&mut hasher);
            (crate::gc::Gc::as_ptr(&list) as usize).hash(&mut hasher);
        }
        // Two enum values are `===` when type and key agree.
        ValueView::Enum { enum_type, key, .. } => {
            17u8.hash(&mut hasher);
            enum_type.hash(&mut hasher);
            key.hash(&mut hasher);
        }
        // `eqv` compares a QuantHash's mutability and its element keys: the
        // keys hash in an order-independent sum, so equal ones share a bucket.
        ValueView::Set(data, mutable) => {
            18u8.hash(&mut hasher);
            mutable.hash(&mut hasher);
            key_set_hash(data.elements.iter()).hash(&mut hasher);
        }
        ValueView::Bag(data, mutable) => {
            19u8.hash(&mut hasher);
            mutable.hash(&mut hasher);
            key_set_hash(data.counts.keys()).hash(&mut hasher);
        }
        ValueView::Mix(data, mutable) => {
            20u8.hash(&mut hasher);
            mutable.hash(&mut hasher);
            key_set_hash(data.weights.keys()).hash(&mut hasher);
        }
        // A Version is `===` by its canonical spelling.
        ValueView::Version { .. } => {
            21u8.hash(&mut hasher);
            value.to_string_value().hash(&mut hasher);
        }
        // A named routine is `===` to another handle with its package and
        // name; an anonymous one only to itself. (A by-name `Routine` handle
        // keeps the scan, so the `&CORE::` arm still meets it.)
        ValueView::Sub(sub) => {
            22u8.hash(&mut hasher);
            if sub.name.resolve().is_empty() {
                (crate::gc::Gc::as_ptr(&sub) as usize).hash(&mut hasher);
            } else {
                sub.package.hash(&mut hasher);
                sub.name.hash(&mut hasher);
            }
        }
        // A type object is `===` by its canonical name; `int` is also
        // `=== 0` and `Nil` the `Nil` value, so those take their buckets.
        ValueView::Package(name) => {
            let name = name.resolve();
            if name == "int" {
                return identity_bucket(&Value::int(0));
            }
            if name == "Nil" {
                return identity_bucket(&Value::NIL);
            }
            23u8.hash(&mut hasher);
            canonical_meta_name(&name).hash(&mut hasher);
        }
        // A mixin (an allomorph `<1>`, `$x but R`) is `===` only to another
        // mixin with a `===` base and the same composition. A mixed type
        // object is also the composed class's type object, so it keeps the scan.
        ValueView::Mixin(inner, mixins) if !matches!(inner.view(), ValueView::Package(_)) => {
            let inner_bucket = identity_bucket(inner)?;
            32u8.hash(&mut hasher);
            inner_bucket.hash(&mut hasher);
            crate::value::types::mixin_identity_key(mixins).hash(&mut hasher);
        }
        ValueView::Nil => 24u8.hash(&mut hasher),
        ValueView::Whatever => 34u8.hash(&mut hasher),
        ValueView::HyperWhatever => 35u8.hash(&mut hasher),
        // A big Rat or FatRat: `eqv` compares the FatRat flag and the value.
        ValueView::BigRat(n, d) => (33u8, value.is_bigfatrat(), n, d).hash(&mut hasher),
        // Code and model objects that are `===` only to the same object.
        ValueView::Regex(_) | ValueView::RegexWithAdverbs(_) => {
            (36u8, value.regex_identity()).hash(&mut hasher)
        }
        ValueView::RakuAst(node) => {
            (37u8, node as *const crate::rakuast::RakuAstNode as usize).hash(&mut hasher)
        }
        ValueView::CustomType(data) => (38u8, data.id).hash(&mut hasher),
        ValueView::Range(a, b) => (25u8, a, b).hash(&mut hasher),
        ValueView::RangeExcl(a, b) => (26u8, a, b).hash(&mut hasher),
        ValueView::RangeExclStart(a, b) => (27u8, a, b).hash(&mut hasher),
        ValueView::RangeExclBoth(a, b) => (28u8, a, b).hash(&mut hasher),
        _ => return None,
    }
    Some(hasher.finish())
}

/// An order-independent hash of a QuantHash's element keys.
// Cost: O(k), k = keys.
fn key_set_hash<'a>(keys: impl Iterator<Item = &'a String>) -> u64 {
    keys.fold(0u64, |sum, key| sum.wrapping_add(str_bucket(key)))
}

/// The public `Metamodel::*HOW` spelling of an internal
/// `Perl6::Metamodel::*HOW` name: `values_identical` treats both as one type.
// Cost: O(|name|).
fn canonical_meta_name(name: &str) -> &str {
    match name.strip_prefix("Perl6::") {
        Some(rest) if rest.starts_with("Metamodel::") && rest.ends_with("HOW") => rest,
        _ => name,
    }
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
