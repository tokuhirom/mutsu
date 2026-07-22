use super::types::allomorph_type_name;
use super::*;

impl Value {
    /// Type-strict structural equivalence (Raku `eqv` operator).
    /// See raku-doc: Language/operators.rakudoc "infix eqv"
    ///
    /// Returns True if two arguments are structurally the same, i.e. from the
    /// same type and (recursively) contain equivalent values.
    /// Unlike PartialEq (used for `==`), this does NOT allow cross-type comparisons:
    ///   1 eqv 1.0  → False  (Int vs Num)
    ///   [1,2] eqv (1,2)  → False  (Array vs List)
    pub(crate) fn eqv(&self, other: &Self) -> bool {
        // Unwrap Scalar/ContainerRef containers: eqv looks through containerization
        if let ValueView::Scalar(inner) = self.view() {
            return inner.eqv(other);
        }
        if let ValueView::Scalar(inner) = other.view() {
            return self.eqv(inner);
        }
        // Deref to an OWNED clone (releasing the cell lock) before recursing:
        // when both sides alias the SAME cell (e.g. two pairs built from the same
        // `key => $var`), holding the lock across the recursive `eqv` would lock
        // the same non-reentrant Mutex twice and deadlock.
        if matches!(self.view(), ValueView::ContainerRef(_)) {
            return self.deref_container().eqv(other);
        }
        if matches!(other.view(), ValueView::ContainerRef(_)) {
            return self.eqv(&other.deref_container());
        }
        // Junction threading: if either side is a junction, thread eqv
        // through it and return the boolean result of the junction.
        if let ValueView::Junction { kind, values } = other.view() {
            let results: Vec<bool> = values.iter().map(|v| self.eqv(v)).collect();
            return match kind {
                crate::value::JunctionKind::Any => results.iter().any(|&b| b),
                crate::value::JunctionKind::All => results.iter().all(|&b| b),
                crate::value::JunctionKind::One => results.iter().filter(|&&b| b).count() == 1,
                crate::value::JunctionKind::None => results.iter().all(|&b| !b),
            };
        }
        if let ValueView::Junction { kind, values } = self.view() {
            let results: Vec<bool> = values.iter().map(|v| v.eqv(other)).collect();
            return match kind {
                crate::value::JunctionKind::Any => results.iter().any(|&b| b),
                crate::value::JunctionKind::All => results.iter().all(|&b| b),
                crate::value::JunctionKind::One => results.iter().filter(|&&b| b).count() == 1,
                crate::value::JunctionKind::None => results.iter().all(|&b| !b),
            };
        }
        match (self.view(), other.view()) {
            // Arrays/Lists: must be same container type (Array vs List) and recursively eqv
            // eqv ignores Scalar wrapping — only Array vs List distinction matters
            (ValueView::Array(a, a_kind), ValueView::Array(b, b_kind)) => {
                a_kind.is_real_array() == b_kind.is_real_array()
                    && a.len() == b.len()
                    && a.iter().zip(b.iter()).all(|(x, y)| x.eqv(y))
            }
            // Hashes: recursively use eqv for values
            (ValueView::Hash(a), ValueView::Hash(b)) => {
                a.len() == b.len() && a.iter().all(|(k, v)| b.get(k).is_some_and(|bv| v.eqv(bv)))
            }
            // Pairs: recursively use eqv for values (Pair and ValuePair are equivalent)
            (ValueView::Pair(ak, av), ValueView::Pair(bk, bv)) => ak == bk && av.eqv(bv),
            (ValueView::ValuePair(ak, av), ValueView::ValuePair(bk, bv)) => {
                ak.eqv(bk) && av.eqv(bv)
            }
            (ValueView::Pair(ak, av), ValueView::ValuePair(bk, bv)) => {
                matches!(bk.view(), ValueView::Str(s) if s.as_str() == ak) && av.eqv(bv)
            }
            (ValueView::ValuePair(ak, av), ValueView::Pair(bk, bv)) => {
                matches!(ak.view(), ValueView::Str(s) if s.as_str() == bk) && av.eqv(bv)
            }
            // Captures: recursively use eqv for positional and named elements
            (
                ValueView::Capture {
                    positional: ap,
                    named: an,
                },
                ValueView::Capture {
                    positional: bp,
                    named: bn,
                },
            ) => {
                ap.len() == bp.len()
                    && ap.iter().zip(bp.iter()).all(|(x, y)| x.eqv(y))
                    && an.len() == bn.len()
                    && an
                        .iter()
                        .all(|(k, v)| bn.get(k).is_some_and(|bv| v.eqv(bv)))
            }
            // Slips: recursively use eqv for elements
            (ValueView::Slip(a), ValueView::Slip(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eqv(y))
            }
            // Seqs: recursively use eqv for elements
            (ValueView::Seq(a), ValueView::Seq(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eqv(y))
            }
            // Num: use bit-exact comparison to distinguish signed zeros,
            // but treat all NaN bit patterns as identical (Raku considers all NaN equal).
            (ValueView::Num(a), ValueView::Num(b)) => {
                if a.is_nan() && b.is_nan() {
                    true
                } else {
                    a.to_bits() == b.to_bits()
                }
            }
            // Complex: use bit-exact comparison for both components,
            // treating all NaN bit patterns as identical.
            (ValueView::Complex(ar, ai), ValueView::Complex(br, bi)) => {
                let re_eq = if ar.is_nan() && br.is_nan() {
                    true
                } else {
                    ar.to_bits() == br.to_bits()
                };
                let im_eq = if ai.is_nan() && bi.is_nan() {
                    true
                } else {
                    ai.to_bits() == bi.to_bits()
                };
                re_eq && im_eq
            }
            // Same-type scalar comparisons delegate to PartialEq
            // BigInt: both sides BigInt — use PartialEq
            (ValueView::BigInt(_), ValueView::BigInt(_)) => self == other,
            // Cross-representation Int/BigInt: compare numerically
            (ValueView::Int(a), ValueView::BigInt(b)) => NumBigInt::from(a) == **b,
            (ValueView::BigInt(a), ValueView::Int(b)) => **a == NumBigInt::from(b),
            // Rat/FatRat: structural equality (n == n, d == d), including NaN (0/0)
            (ValueView::Rat(n1, d1), ValueView::Rat(n2, d2))
            | (ValueView::FatRat(n1, d1), ValueView::FatRat(n2, d2)) => n1 == n2 && d1 == d2,
            // Sets: eqv must distinguish elements that share a string key but
            // differ in type — specifically an allomorph (e.g. the IntStr <42>)
            // from a plain value (Int 42), which rakudo separates by `.WHICH`.
            // We compare the allomorph kind of each typed element rather than a
            // full type-strict eqv, because mutsu does not always retain a Set
            // element's exact numeric type (a Rat element can fall back to its
            // Str key), and a full eqv would wrongly split two equal Rat sets.
            // Mutability (Set vs SetHash) is part of the type, so eqv must
            // distinguish them (`Set.new(42) eqv SetHash.new(42)` is False).
            // Raku's set operators (`(|)`/`(&)` etc.) always yield an immutable
            // Set regardless of operand mutability, so comparing the flag here
            // matches values produced by those operators too.
            (ValueView::Set(a, a_mut), ValueView::Set(b, b_mut)) => {
                fn allomorph_kind(v: &Value) -> Option<String> {
                    match v.view() {
                        ValueView::Mixin(inner, mixins) => allomorph_type_name(inner, mixins),
                        _ => None,
                    }
                }
                a_mut == b_mut
                    && a.elements.len() == b.elements.len()
                    && a.elements.iter().all(|k| {
                        b.elements.contains(k)
                            && allomorph_kind(&a.typed_key(k)) == allomorph_kind(&b.typed_key(k))
                    })
            }
            // Bag/Mix: like Set, eqv distinguishes the immutable variant from
            // its mutable QuantHash (Bag vs BagHash, Mix vs MixHash). The data
            // comparison is delegated to PartialEq, which ignores the flag.
            (ValueView::Bag(a, a_mut), ValueView::Bag(b, b_mut)) => a_mut == b_mut && *a == *b,
            (ValueView::Mix(a, a_mut), ValueView::Mix(b, b_mut)) => a_mut == b_mut && *a == *b,
            // Two big rationals are eqv only when they share the FatRat flag
            // (a big Rat is never eqv to a big FatRat, even with equal value).
            (ValueView::BigRat(_, _), ValueView::BigRat(_, _)) => {
                self.is_bigfatrat() == other.is_bigfatrat() && self == other
            }
            (ValueView::Int(_), ValueView::Int(_))
            | (ValueView::Str(_), ValueView::Str(_))
            | (ValueView::Bool(_), ValueView::Bool(_))
            | (ValueView::Enum { .. }, ValueView::Enum { .. })
            | (ValueView::Regex(_), ValueView::Regex(_))
            | (ValueView::RegexWithAdverbs { .. }, ValueView::RegexWithAdverbs { .. })
            | (ValueView::Routine { .. }, ValueView::Routine { .. }) => self == other,
            (ValueView::Sub(a), ValueView::Sub(b)) => {
                if crate::gc::Gc::ptr_eq(&a, &b) {
                    return true;
                }
                let a_name = a.name.resolve();
                let b_name = b.name.resolve();
                !a_name.is_empty() && a_name == b_name && a.package == b.package
            }
            // Signature instances: compare by .raku string (structural equality)
            (
                ValueView::Instance {
                    class_name: cn_a, ..
                },
                ValueView::Instance {
                    class_name: cn_b, ..
                },
            ) if cn_a == "Signature" && cn_b == "Signature" => {
                let raku_a = if let ValueView::Instance { attributes, .. } = self.view() {
                    attributes.as_map().get("raku").map(|v| v.to_string_value())
                } else {
                    None
                };
                let raku_b = if let ValueView::Instance { attributes, .. } = other.view() {
                    attributes.as_map().get("raku").map(|v| v.to_string_value())
                } else {
                    None
                };
                raku_a == raku_b
            }
            (
                ValueView::Instance {
                    class_name: cn_a,
                    attributes: a_attrs,
                    ..
                },
                ValueView::Instance {
                    class_name: cn_b,
                    attributes: b_attrs,
                    ..
                },
            ) if cn_a == cn_b
                && a_attrs.contains_key("year")
                && a_attrs.contains_key("month")
                && a_attrs.contains_key("day")
                && a_attrs.contains_key("hour")
                && a_attrs.contains_key("minute")
                && a_attrs.contains_key("second")
                && a_attrs.contains_key("timezone")
                && b_attrs.contains_key("year")
                && b_attrs.contains_key("month")
                && b_attrs.contains_key("day")
                && b_attrs.contains_key("hour")
                && b_attrs.contains_key("minute")
                && b_attrs.contains_key("second")
                && b_attrs.contains_key("timezone") =>
            {
                let (ay, am, ad, ah, amin, asec, atz) =
                    crate::builtins::methods_0arg::temporal::datetime_attrs(&(a_attrs).as_map());
                let (by, bm, bd, bh, bmin, bsec, btz) =
                    crate::builtins::methods_0arg::temporal::datetime_attrs(&(b_attrs).as_map());
                ay == by
                    && am == bm
                    && ad == bd
                    && ah == bh
                    && amin == bmin
                    && atz == btz
                    && (asec - bsec).abs() < 1e-6
            }
            // Date instances: compare only year/month/day (ignore formatter attrs)
            (
                ValueView::Instance {
                    class_name: cn_a,
                    attributes: a_attrs,
                    ..
                },
                ValueView::Instance {
                    class_name: cn_b,
                    attributes: b_attrs,
                    ..
                },
            ) if cn_a == cn_b
                && a_attrs.contains_key("year")
                && a_attrs.contains_key("month")
                && a_attrs.contains_key("day")
                && !a_attrs.contains_key("hour")
                && b_attrs.contains_key("year")
                && b_attrs.contains_key("month")
                && b_attrs.contains_key("day")
                && !b_attrs.contains_key("hour") =>
            {
                let (ay, am, ad) =
                    crate::builtins::methods_0arg::temporal::date_attrs(&(a_attrs).as_map());
                let (by, bm, bd) =
                    crate::builtins::methods_0arg::temporal::date_attrs(&(b_attrs).as_map());
                ay == by && am == bm && ad == bd
            }
            // StrDistance instances: structural equality on before/after
            (
                ValueView::Instance {
                    class_name: cn_a,
                    attributes: a_attrs,
                    ..
                },
                ValueView::Instance {
                    class_name: cn_b,
                    attributes: b_attrs,
                    ..
                },
            ) if cn_a == "StrDistance" && cn_b == "StrDistance" => {
                let before_eq = match (
                    a_attrs.as_map().get("before"),
                    b_attrs.as_map().get("before"),
                ) {
                    (Some(a), Some(b)) => a.eqv(b),
                    (None, None) => true,
                    _ => false,
                };
                let after_eq = match (a_attrs.as_map().get("after"), b_attrs.as_map().get("after"))
                {
                    (Some(a), Some(b)) => a.eqv(b),
                    (None, None) => true,
                    _ => false,
                };
                before_eq && after_eq
            }
            // Other Instance types: use identity comparison
            (ValueView::Instance { .. }, ValueView::Instance { .. }) => self == other,
            // Nil and Package("Any") are eqv: both represent the undefined type object Any.
            // This matters for containerized contexts (e.g. hash values).
            (ValueView::Nil, ValueView::Package(name))
            | (ValueView::Package(name), ValueView::Nil)
                if name == "Any" =>
            {
                true
            }
            (ValueView::Range(_, _), ValueView::Range(_, _))
            | (ValueView::RangeExcl(_, _), ValueView::RangeExcl(_, _))
            | (ValueView::RangeExclStart(_, _), ValueView::RangeExclStart(_, _))
            | (ValueView::RangeExclBoth(_, _), ValueView::RangeExclBoth(_, _))
            | (ValueView::GenericRange { .. }, ValueView::GenericRange { .. })
            | (ValueView::LazyList(_), ValueView::LazyList(_))
            | (ValueView::Version { .. }, ValueView::Version { .. })
            | (ValueView::Nil, ValueView::Nil)
            | (ValueView::Package(_), ValueView::Package(_))
            | (ValueView::CompUnitDepSpec { .. }, ValueView::CompUnitDepSpec { .. })
            | (ValueView::Junction { .. }, ValueView::Junction { .. })
            | (ValueView::Promise(_), ValueView::Promise(_))
            | (ValueView::Channel(_), ValueView::Channel(_))
            | (ValueView::Uni { .. }, ValueView::Uni { .. }) => self == other,
            // Mixin with only the read-only topic marker is transparent
            // (used by `with literal { ... }` to flag immutable $_).
            (ValueView::Mixin(inner, mix), _)
                if mix.len() == 1 && mix.contains_key("__mutsu_topic_ro__") =>
            {
                inner.eqv(other)
            }
            (_, ValueView::Mixin(inner, mix))
                if mix.len() == 1 && mix.contains_key("__mutsu_topic_ro__") =>
            {
                self.eqv(inner)
            }
            // Mixin (allomorphs): compare both base values and mixin maps with eqv
            (ValueView::Mixin(a, a_mix), ValueView::Mixin(b, b_mix)) => {
                if !a.eqv(b) {
                    return false;
                }
                // Compare mixin maps (e.g. Str part of allomorphs)
                if a_mix.len() != b_mix.len() {
                    return false;
                }
                a_mix
                    .iter()
                    .all(|(k, v)| b_mix.get(k).is_some_and(|bv| v.eqv(bv)))
            }
            // Cross-type comparisons always return false for eqv
            _ => false,
        }
    }
}
