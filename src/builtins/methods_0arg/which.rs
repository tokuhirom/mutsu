//! `.WHICH`: the object-identity string of a value (`Int|5`, `Str|a`,
//! `Array|12`, ...), as a `ValueObjAt` or an `ObjAt`.
//!
//! One implementation for every layer: the cascade's `WHICH` arm and the
//! `WHICH` rows of the method table (ADR-11276) both call [`which_of`].

use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// Whether `v`'s own identity is VALUE-based (`ValueObjAt`, a content digest)
/// rather than object-based (`ObjAt`, the allocation).
///
/// For a `Pair` this is not a property of the Pair but of what it holds, and
/// rakudo is explicit about it: `(foo => 100).WHICH` is a `ValueObjAt` digest,
/// while `(foo => [1, 2]).WHICH` and `(foo => $v).WHICH` (a container-held
/// value) are the object's address. The reason is that a reference type or a
/// container can change under the pair, so a content digest would be a lie --
/// which is exactly what roast's "Clone of Pair does not share .WHICH"
/// (rakudo 5031dab3ac) pins: `$p.clone.WHICH !=== $p.WHICH` when the value is a
/// container, both before and after the container is written to.
///
/// Measured against raku v2026.07: `Int`/`Str`/`Rat`/`Set`/`Pair`/a type object
/// answer `ValueObjAt`; `Array`/`List`/`Hash`/a `Scalar`-held value answer
/// `ObjAt`.
fn has_value_identity(v: &Value) -> bool {
    match v.view() {
        // A `Pair`'s Str key is always value-identified; only the value decides.
        ValueView::Pair(_, val) => has_value_identity(val),
        ValueView::ValuePair(k, val) => has_value_identity(k) && has_value_identity(val),
        _ => matches!(
            v.view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Str(_)
                | ValueView::Bool(_)
                | ValueView::Rat(_, _)
                | ValueView::BigRat(_, _)
                | ValueView::FatRat(_, _)
                | ValueView::Complex(_, _)
                | ValueView::Set(_, _)
                | ValueView::Bag(_, _)
                | ValueView::Mix(_, _)
                | ValueView::Junction { .. }
                | ValueView::Nil
                | ValueView::Enum { .. }
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
                | ValueView::Version { .. }
                | ValueView::Package(_)
                | ValueView::CustomType(_)
        ),
    }
}

/// The identity of `target`: `ValueObjAt` for a value type (content-keyed),
/// `ObjAt` for a reference type.
// Cost: O(1) for a scalar or a container (a per-container id); O(n) for a
// `Set`/`Bag`/`Mix` (n = elements, hashed) and for a `Range`/`Version`
// (n = chars of the rendering).
pub(crate) fn which_of(target: &Value) -> Value {
    // A plain Str's identity is `Str|<text>`. The ValueObjAt keeps the
    // invocant itself (shared) and renders that text only when it is
    // read -- see `AttrMap::objat_which`.
    // Cost: O(1) for a plain Str.
    if target.is_str_value() {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(crate::value::OBJAT_STR_PAYLOAD.to_string(), target.clone());
        return Value::make_instance(Symbol::intern("ValueObjAt"), attrs);
    }
    // An ObjAt is identified by the identity it carries, not by the
    // object that happens to hold it: `$o.WHICH.WHICH` is
    // `ObjAt|<$o.WHICH>` on every call (and `ValueObjAt|Str|a` for
    // `"a".WHICH.WHICH`), so two ObjAts of one object are the same
    // Set/Bag key. The class is kept, as in Rakudo.
    // Cost: O(n), n = length of the carried identity string.
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && matches!(class_name.resolve().as_str(), "ObjAt" | "ValueObjAt")
    {
        let inner = attributes.as_map().objat_which().unwrap_or_default();
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "WHICH".to_string(),
            Value::str(format!("{}|{}", class_name.resolve(), inner)),
        );
        return Value::make_instance(class_name, attrs);
    }
    // Determine if this is a value type (ValueObjAt) or reference type (ObjAt)
    let is_value_type = has_value_identity(target);
    let which_str = match target.view() {
        ValueView::Package(name) => format!(
            "{}|U{}",
            crate::value::user_facing_type_name(&name.resolve()),
            name.id()
        ),
        ValueView::CustomType(c) => {
            format!(
                "{}|U{}",
                crate::value::user_facing_type_name(&c.name.resolve()),
                c.id
            )
        }
        ValueView::Int(n) => format!("Int|{}", n),
        ValueView::BigInt(n) => format!("Int|{}", *n),
        ValueView::Num(n) => format!("Num|{}", n),
        // A Str subclass/mixin that reaches here (the plain Str took the
        // shared-payload branch above). Cost: O(n), n = chars.
        ValueView::Str(s) => format!("Str|{}", *s),
        ValueView::Bool(b) => format!("Bool|{}", if b { 1 } else { 0 }),
        ValueView::Rat(n, d) => format!("Rat|{}/{}", n, d),
        ValueView::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
        ValueView::BigRat(n, d) => {
            let flavour = if target.is_bigfatrat() {
                "FatRat"
            } else {
                "Rat"
            };
            format!("{}|{}/{}", flavour, n, d)
        }
        ValueView::Complex(r, i) => format!("Complex|{}+{}i", r, i),
        // An enum value's identity is `{EnumType}|{ordinal}` (raku:
        // `Bob.WHICH` is `Names|0`, using the position in the enum, not
        // the underlying value). Without this arm an enum fell to the
        // global-counter fallback, giving a non-deterministic `Int|N`
        // that broke `Bob.WHICH eqv Bob.WHICH`.
        ValueView::Enum {
            enum_type, index, ..
        } => format!("{}|{}", enum_type.resolve(), index),
        ValueView::Nil => format!("Nil|U{}", Symbol::intern("Nil").id()),
        // A Version's identity is its canonical string (raku:
        // `v1.02.3.WHICH` is `Version|1.02.3`), so two versions
        // spelled differently are NOT `===` even when they `==`.
        ValueView::Version { .. } => format!("Version|{}", target.to_string_value()),
        ValueView::Set(set, _) => {
            let mut keys: Vec<&String> = set.iter().collect();
            keys.sort();
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            for k in &keys {
                k.hash(&mut hasher);
            }
            format!("Set|{:016X}", hasher.finish())
        }
        ValueView::Bag(bag, _) => {
            let mut pairs: Vec<(&String, &num_bigint::BigInt)> = bag.iter().collect();
            pairs.sort_by(|a, b| a.0.cmp(b.0));
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            for (k, v) in &pairs {
                k.hash(&mut hasher);
                v.hash(&mut hasher);
            }
            format!("Bag|{:016X}", hasher.finish())
        }
        ValueView::Mix(mix, _) => {
            let mut pairs: Vec<(&String, &f64)> = mix.iter().collect();
            pairs.sort_by(|a, b| a.0.cmp(b.0));
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            for (k, v) in &pairs {
                k.hash(&mut hasher);
                v.to_bits().hash(&mut hasher);
            }
            format!("Mix|{:016X}", hasher.finish())
        }
        ValueView::Sub(sub_data) => {
            format!(
                "{}|{}",
                runtime::utils::value_type_name(target),
                sub_data.id
            )
        }
        // One identity with the object-hash key (`value_which_key`):
        // the code object's payload id, not the pattern text's address
        // (which every evaluation of a literal shares).
        ValueView::Regex(_) | ValueView::RegexWithAdverbs(_) => {
            runtime::utils::value_which_key(target)
        }
        ValueView::Instance { class_name, id, .. } => {
            // Anonymous classes display as `<anon|N>` (the instance id
            // keeps the identity unique).
            format!(
                "{}|{}",
                crate::value::user_facing_type_name(&class_name.resolve()),
                id
            )
        }
        ValueView::Junction { kind, values } => {
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            // Hash the kind
            match kind {
                crate::value::JunctionKind::Any => 0u8.hash(&mut hasher),
                crate::value::JunctionKind::All => 1u8.hash(&mut hasher),
                crate::value::JunctionKind::One => 2u8.hash(&mut hasher),
                crate::value::JunctionKind::None => 3u8.hash(&mut hasher),
            }
            // Hash each eigenstate's string representation
            for v in values.iter() {
                v.to_string_value().hash(&mut hasher);
            }
            format!("Junction|{:016X}", hasher.finish())
        }
        // Never-reused id, like the Array/Hash arms below.
        ValueView::Seq(items) => {
            format!("Seq|{}", items.which_id())
        }
        // A `Slip`'s payload is a bare `Arc<Vec<Value>>` with nowhere
        // to embed the id the other containers carry, so its
        // never-reused id comes from the weak-checked side table.
        ValueView::Slip(items) => {
            format!("Slip|{}", crate::value::which_id::slip_which_id(&items))
        }
        ValueView::RakuAst(node) => {
            // RakuAST nodes are reference-like model objects. The
            // Arc allocation is stable across Value clones, so its
            // address supplies the node identity used by `.WHICH`.
            format!(
                "{}|{}",
                node.class.printed_name(),
                node as *const _ as usize
            )
        }
        // A lazily minted, never-reused id rather than the container's
        // ADDRESS: a `.WHICH` string always outlives its object, and an
        // address is unique only among LIVE objects, so two dead
        // temporaries collided whenever the allocator handed the second
        // one the block the first had just freed
        // (`[1,2].WHICH eq [3,4,5].WHICH` was True). See
        // `crate::value::which_id`.
        // Cost: O(1) (a per-container id, no element walk).
        ValueView::Array(items, ..) => {
            format!("Array|{}", items.which_id.get())
        }
        ValueView::Hash(map) => {
            format!("Hash|{}", map.which_id.get())
        }
        // A Pair whose contents are themselves value-identified is
        // value-identified too, from the key's and value's own `.WHICH`
        // (raku: `(a => 1).WHICH eq (a => 1).WHICH`). Without this the
        // Pair fell to the global-counter tail below and every read
        // minted a fresh, unstable id -- so two structurally identical
        // pairs never matched, and the string was not even stable across
        // two reads of the SAME pair.
        //
        // `value_which_key` already carries the correct encoding (it is
        // what Set/Bag/Mix element keying uses, and it recurses through
        // the key and value), so delegate rather than invent a second
        // one.
        //
        // A pair holding a CONTAINER or a reference type keeps object
        // identity and falls through to the tail -- see
        // `has_value_identity`.
        ValueView::Pair(_, _) | ValueView::ValuePair(_, _) if is_value_type => {
            runtime::utils::value_which_key(target)
        }
        // Never-reused ids, for the same reason as the Array/Hash arms
        // above: `Promise.new.WHICH eq Promise.new.WHICH` was `True`
        // whenever the second temporary landed on the first's freed
        // block.
        ValueView::Promise(p) => {
            format!("Promise|{}", p.which_id())
        }
        ValueView::Channel(c) => {
            format!("Channel|{}", c.which_id())
        }
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => {
            // A Range is a value type: its identity is its notation
            // (`Range|1..2`, `Range|1^..^5`, `Range|"a".."z"`), which is
            // exactly the range's `.raku` representation.
            format!("Range|{}", super::raku_repr::raku_value(target))
        }
        ValueView::Whatever => {
            use std::sync::atomic::{AtomicU64, Ordering};
            static WHATEVER_ID: AtomicU64 = AtomicU64::new(0);
            // Whatever is a type object singleton
            let id = WHATEVER_ID.load(Ordering::Relaxed);
            if id == 0 {
                WHATEVER_ID.store(1, Ordering::Relaxed);
            }
            format!("Whatever|U{}", WHATEVER_ID.load(Ordering::Relaxed))
        }
        _ => {
            // Fallback: use a global counter to ensure uniqueness
            // This is not ideal since the same value will get different IDs
            // on repeated calls, but it prevents false identity collisions.
            use std::sync::atomic::{AtomicU64, Ordering};
            static COUNTER: AtomicU64 = AtomicU64::new(1);
            format!(
                "{}|{}",
                runtime::utils::value_type_name(target),
                COUNTER.fetch_add(1, Ordering::Relaxed)
            )
        }
    };
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("WHICH".to_string(), Value::str(which_str));
    let objat_class = if is_value_type { "ValueObjAt" } else { "ObjAt" };
    Value::make_instance(Symbol::intern(objat_class), attrs)
}
