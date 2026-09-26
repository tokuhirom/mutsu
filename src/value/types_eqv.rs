use super::*;

/// How [`Value::eqv`] compares two instances of the same user class.
pub(crate) enum InstanceEqv {
    /// The answer is already known (the same object).
    Decided(bool),
    /// A user `raku` method rendered both sides and the strings decided it.
    /// Everything that method renders — role attributes carried by a punned
    /// role's mixin included — is covered by the answer.
    Rendered(bool),
    /// Compare attribute by attribute, skipping the listed private attribute
    /// names: Rakudo's default `.raku` renders public attributes only, so a
    /// private-only difference is invisible to `eqv`.
    Public(std::rc::Rc<[String]>),
    /// No class metadata: compare every attribute slot.
    Structural,
}

/// Decides same-class user-instance pairs for [`Value::eqv_with`].
pub(crate) trait EqvInstanceHook {
    fn instance_eqv(&mut self, a: &Value, b: &Value) -> InstanceEqv;
}

/// The interpreter-free hook: every attribute slot takes part.
pub(crate) struct StructuralInstances;

impl EqvInstanceHook for StructuralInstances {
    fn instance_eqv(&mut self, _a: &Value, _b: &Value) -> InstanceEqv {
        InstanceEqv::Structural
    }
}

struct EqvCtx<'h> {
    /// Instance-identity pairs already under comparison (cycle guard).
    pairs: std::collections::HashSet<(u64, u64)>,
    hook: &'h mut dyn EqvInstanceHook,
}

impl Value {
    /// Type-strict structural equivalence (Raku `eqv` operator).
    /// See raku-doc: Language/operators.rakudoc "infix eqv"
    ///
    /// Returns True if two arguments are structurally the same, i.e. from the
    /// same type and (recursively) contain equivalent values.
    /// Unlike PartialEq (used for `==`), this does NOT allow cross-type comparisons:
    ///   1 eqv 1.0  → False  (Int vs Num)
    ///   `[1,2] eqv (1,2)`  → False  (Array vs List)
    pub(crate) fn eqv(&self, other: &Self) -> bool {
        self.eqv_with(other, &mut StructuralInstances)
    }

    /// [`Value::eqv`] with an [`EqvInstanceHook`] consulted for every pair of
    /// same-class user instances, at any depth (inside arrays, hashes, pairs,
    /// other instances' attributes). The VM's `infix:<eqv>` passes a hook that
    /// applies Rakudo's `Any:D eqv Any:D` rule (same `.WHAT` and equal
    /// `.raku`), which needs the interpreter; this pure walk cannot.
    pub(crate) fn eqv_with(&self, other: &Self, hook: &mut dyn EqvInstanceHook) -> bool {
        let mut seen = EqvCtx {
            pairs: std::collections::HashSet::new(),
            hook,
        };
        self.eqv_inner(other, &mut seen)
    }

    fn eqv_inner(&self, other: &Self, seen: &mut EqvCtx<'_>) -> bool {
        // Unwrap Scalar/ContainerRef containers: eqv looks through containerization
        if let ValueView::Scalar(inner) = self.view() {
            return inner.eqv_inner(other, seen);
        }
        if let ValueView::Scalar(inner) = other.view() {
            return self.eqv_inner(inner, seen);
        }
        // Deref to an OWNED clone (releasing the cell lock) before recursing:
        // when both sides alias the SAME cell (e.g. two pairs built from the same
        // `key => $var`), holding the lock across the recursive `eqv` would lock
        // the same non-reentrant Mutex twice and deadlock.
        if matches!(self.view(), ValueView::ContainerRef(_)) {
            return self.deref_container().eqv_inner(other, seen);
        }
        if matches!(other.view(), ValueView::ContainerRef(_)) {
            return self.eqv_inner(&other.deref_container(), seen);
        }
        // A deferred vivification token (an out-of-range `:=`-bound element, or a
        // multi-dim subscript whose path does not exist yet) is a container
        // wrapper like the two above, not a value: compare the hole value it
        // reads back as, never the token itself.
        if matches!(self.view(), ValueView::HashEntryRef { .. }) {
            return self.hash_entry_read().eqv_inner(other, seen);
        }
        if matches!(other.view(), ValueView::HashEntryRef { .. }) {
            return self.eqv_inner(&other.hash_entry_read(), seen);
        }
        // User objects can contain back-references (for example a
        // configuration owns its evaluator while the evaluator owns the
        // configuration). Treat a repeated pair of object identities as an
        // already-validated cycle instead of descending forever.
        if let (ValueView::Instance { id: a, .. }, ValueView::Instance { id: b, .. }) =
            (self.view(), other.view())
            && !seen.pairs.insert((a, b))
        {
            return true;
        }
        // ADR-0038 S2: `.cache` on a not-yet-reified `Seq` hands back a SECOND
        // handle over the same body tagged `SeqView::List`. That handle
        // IS a `List` as far as Raku is concerned (`value_type_name` says so),
        // and `eqv` is type-strict, so it must compare as one — otherwise
        // `Seq.new($iter).cache eqv (1, 2, 3)` answers False (and, worse,
        // `... eqv (1, 2, 3).Seq` answers True). Normalising here rather than
        // adding four cross-arms keeps every pairing (List-view vs List,
        // vs Array, vs real Seq, vs List-view) consistent in one place.
        if let Some(as_list) = Self::seq_list_view_as_list(self) {
            return as_list.eqv_inner(other, seen);
        }
        if let Some(as_list) = Self::seq_list_view_as_list(other) {
            return self.eqv_inner(&as_list, seen);
        }
        // Junction threading: if either side is a junction, thread eqv
        // through it and return the boolean result of the junction.
        if let ValueView::Junction { kind, values } = other.view() {
            let results: Vec<bool> = values.iter().map(|v| self.eqv_inner(v, seen)).collect();
            return match kind {
                crate::value::JunctionKind::Any => results.iter().any(|&b| b),
                crate::value::JunctionKind::All => results.iter().all(|&b| b),
                crate::value::JunctionKind::One => results.iter().filter(|&&b| b).count() == 1,
                crate::value::JunctionKind::None => results.iter().all(|&b| !b),
            };
        }
        if let ValueView::Junction { kind, values } = self.view() {
            let results: Vec<bool> = values.iter().map(|v| v.eqv_inner(other, seen)).collect();
            return match kind {
                crate::value::JunctionKind::Any => results.iter().any(|&b| b),
                crate::value::JunctionKind::All => results.iter().all(|&b| b),
                crate::value::JunctionKind::One => results.iter().filter(|&&b| b).count() == 1,
                crate::value::JunctionKind::None => results.iter().all(|&b| !b),
            };
        }
        match (self.view(), other.view()) {
            // Whatever and HyperWhatever are tag values rather than ordinary
            // PartialEq scalars, so `self == other` is intentionally not the
            // equivalence test for these singleton values.
            (ValueView::Whatever, ValueView::Whatever)
            | (ValueView::HyperWhatever, ValueView::HyperWhatever) => true,
            // Arrays/Lists: must be same container type (Array vs List) and recursively eqv
            // eqv ignores Scalar wrapping — only Array vs List distinction matters
            // Cost: O(1) on a kind/length mismatch, else O(i) to the first differing
            // element (recursing into it), O(e) when equal.
            (ValueView::Array(a, a_kind), ValueView::Array(b, b_kind)) => {
                a_kind.is_real_array() == b_kind.is_real_array()
                    && a.len() == b.len()
                    && a.iter().zip(b.iter()).all(|(x, y)| x.eqv_inner(y, seen))
            }
            // Buffer storage: bytes and element type, same rule as `==`.
            (ValueView::BufStorage(a), ValueView::BufStorage(b)) => *a == *b,
            // Hashes: recursively use eqv for values
            (ValueView::Hash(a), ValueView::Hash(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .all(|(k, v)| b.get(k).is_some_and(|bv| v.eqv_inner(bv, seen)))
            }
            // Pairs: recursively use eqv for values (Pair and ValuePair are equivalent)
            (ValueView::Pair(ak, av), ValueView::Pair(bk, bv)) => {
                ak == bk && av.eqv_inner(bv, seen)
            }
            (ValueView::ValuePair(ak, av), ValueView::ValuePair(bk, bv)) => {
                ak.eqv_inner(bk, seen) && av.eqv_inner(bv, seen)
            }
            (ValueView::Pair(ak, av), ValueView::ValuePair(bk, bv)) => {
                matches!(bk.view(), ValueView::Str(s) if s.as_str() == ak) && av.eqv_inner(bv, seen)
            }
            (ValueView::ValuePair(ak, av), ValueView::Pair(bk, bv)) => {
                matches!(ak.view(), ValueView::Str(s) if s.as_str() == bk) && av.eqv_inner(bv, seen)
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
                    && ap.iter().zip(bp.iter()).all(|(x, y)| x.eqv_inner(y, seen))
                    && an.len() == bn.len()
                    && an
                        .iter()
                        .all(|(k, v)| bn.get(k).is_some_and(|bv| v.eqv_inner(bv, seen)))
            }
            // Slips: recursively use eqv for elements
            (ValueView::Slip(a), ValueView::Slip(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eqv_inner(y, seen))
            }
            // Seqs: recursively use eqv for elements
            (ValueView::Seq(a), ValueView::Seq(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eqv_inner(y, seen))
            }
            // RakuAST nodes are model objects: eqv compares their complete
            // immutable tree, not their Arc identity. This lets two
            // separately constructed copies of the same node compare eqv,
            // while `values_identical` still distinguishes their identity.
            (ValueView::RakuAst(a), ValueView::RakuAst(b)) => {
                a.class == b.class
                    && a.fields.len() == b.fields.len()
                    && a.fields.iter().zip(b.fields.iter()).all(|(af, bf)| {
                        af.name == bf.name
                            && match (&af.value, &bf.value) {
                                (
                                    crate::rakuast::RakuAstFieldValue::Node(av),
                                    crate::rakuast::RakuAstFieldValue::Node(bv),
                                ) => av.eqv(bv),
                                (
                                    crate::rakuast::RakuAstFieldValue::List(av),
                                    crate::rakuast::RakuAstFieldValue::List(bv),
                                ) => {
                                    av.len() == bv.len()
                                        && av.iter().zip(bv.iter()).all(|(x, y)| x.eqv(y))
                                }
                                (
                                    crate::rakuast::RakuAstFieldValue::Adverb(av),
                                    crate::rakuast::RakuAstFieldValue::Adverb(bv),
                                ) => av == bv,
                                _ => false,
                            }
                    })
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
            // Sets: the element store is `.WHICH`-keyed, so comparing the key
            // sets IS element-identity comparison (an IntStr <42> and an Int 42
            // occupy different keys, matching rakudo's `.WHICH` separation).
            // Mutability (Set vs SetHash) is part of the type, so eqv must
            // distinguish them (`Set.new(42) eqv SetHash.new(42)` is False).
            // Raku's set operators (`(|)`/`(&)` etc.) always yield an immutable
            // Set regardless of operand mutability, so comparing the flag here
            // matches values produced by those operators too.
            (ValueView::Set(a, a_mut), ValueView::Set(b, b_mut)) => {
                a_mut == b_mut && a.elements == b.elements
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
            // ObjAt / ValueObjAt: equal identity text. `Str.WHICH` keeps its
            // invocant rather than a rendered `WHICH` key, so the attribute
            // maps of two equal ObjAts need not match (see
            // `AttrMap::objat_which`).
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
            ) if cn_a == cn_b && (cn_a == "ObjAt" || cn_a == "ValueObjAt") => {
                a_attrs.as_map().objat_which() == b_attrs.as_map().objat_which()
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
                    (Some(a), Some(b)) => a.eqv_inner(b, seen),
                    (None, None) => true,
                    _ => false,
                };
                let after_eq = match (a_attrs.as_map().get("after"), b_attrs.as_map().get("after"))
                {
                    (Some(a), Some(b)) => a.eqv_inner(b, seen),
                    (None, None) => true,
                    _ => false,
                };
                before_eq && after_eq
            }
            // Two Match objects compare by CONTENT, across classes: a grammar
            // cursor reports the grammar's own class (raku: `Grammar` IS a
            // `Match` subclass), yet raku holds
            // `EVAL(G.parse($s).raku) eqv G.parse($s)` — the `.raku` of a cursor
            // renders as a plain `Match.new(...)`, and the round trip is `eqv`
            // to the cursor it came from (roast: `S05-match/raku.t`, verified
            // against raku 2026-08-27). The generic Instance arm below would
            // compare class names and attribute maps, which differ in both the
            // class and the internal cursor marker.
            (ValueView::Instance { .. }, ValueView::Instance { .. })
                if self.is_match_instance() && other.is_match_instance() =>
            {
                let scalars_eq = self.match_from() == other.match_from()
                    && self.match_to() == other.match_to()
                    && self.match_orig().map(|v| v.to_string_value())
                        == other.match_orig().map(|v| v.to_string_value());
                let mut opt_eqv = |a: Option<Value>, b: Option<Value>| match (a, b) {
                    (Some(a), Some(b)) => a.eqv_inner(&b, seen),
                    (None, None) => true,
                    _ => false,
                };
                scalars_eq
                    && opt_eqv(self.match_list(), other.match_list())
                    && opt_eqv(self.match_named(), other.match_named())
            }
            // IO::Handle's open registry id and mode are process-local state;
            // only its public, reproducible attributes participate in eqv.
            (
                ValueView::Instance {
                    class_name: a_class,
                    attributes: a_attrs,
                    ..
                },
                ValueView::Instance {
                    class_name: b_class,
                    attributes: b_attrs,
                    ..
                },
            ) if a_class == "IO::Handle" && b_class == "IO::Handle" => {
                const COMPARABLE_KEYS: &[&str] =
                    &["path", "chomp", "nl-in", "nl-out", "encoding", "bin"];
                let a_map = a_attrs.as_map();
                let b_map = b_attrs.as_map();
                COMPARABLE_KEYS
                    .iter()
                    .all(|key| match (a_map.get(*key), b_map.get(*key)) {
                        (Some(a), Some(b)) => a.eqv_inner(b, seen),
                        (None, None) => true,
                        _ => false,
                    })
            }
            // Duration values are numeric quantities. The arithmetic path may
            // retain a Num while the constructor retains an exact Rat, but
            // Rakudo compares equal durations by their numeric seconds.
            (
                ValueView::Instance {
                    class_name: a_class,
                    attributes: a_attrs,
                    ..
                },
                ValueView::Instance {
                    class_name: b_class,
                    attributes: b_attrs,
                    ..
                },
            ) if a_class == "Duration" && b_class == "Duration" => {
                let a_seconds = a_attrs
                    .as_map()
                    .get("value")
                    .map(Value::to_f64)
                    .unwrap_or(0.0);
                let b_seconds = b_attrs
                    .as_map()
                    .get("value")
                    .map(Value::to_f64)
                    .unwrap_or(0.0);
                a_seconds == b_seconds
            }
            // Instants are numeric TAI values. Arithmetic can preserve an
            // exact Rat on one side while constructing a Num on the other;
            // compare their numeric timestamps, as Raku does for eqv.
            (
                ValueView::Instance {
                    class_name: a_class,
                    attributes: a_attrs,
                    ..
                },
                ValueView::Instance {
                    class_name: b_class,
                    attributes: b_attrs,
                    ..
                },
            ) if a_class == "Instant" && b_class == "Instant" => {
                let a_seconds = a_attrs
                    .as_map()
                    .get("value")
                    .map(Value::to_f64)
                    .unwrap_or(0.0);
                let b_seconds = b_attrs
                    .as_map()
                    .get("value")
                    .map(Value::to_f64)
                    .unwrap_or(0.0);
                a_seconds == b_seconds
            }
            // User instances compare by their public attribute values. Rakudo
            // uses this structural object equivalence for `eqv`/`is-deeply`;
            // comparing the backing maps with `eqv` also makes independently
            // constructed objects containing routines compare correctly (their
            // routine identities are not part of the object's value).
            (
                ValueView::Instance {
                    class_name: a_class,
                    attributes: a_attrs,
                    ..
                },
                ValueView::Instance {
                    class_name: b_class,
                    attributes: b_attrs,
                    ..
                },
            ) => {
                if a_class != b_class {
                    return false;
                }
                let private = match seen.hook.instance_eqv(self, other) {
                    InstanceEqv::Decided(answer) | InstanceEqv::Rendered(answer) => return answer,
                    InstanceEqv::Public(private) => Some(private),
                    InstanceEqv::Structural => None,
                };
                let a_map = a_attrs.to_map();
                let b_map = b_attrs.to_map();
                // The full object constructor materializes inherited/redeclared
                // attributes under `Class\0name` keys, while native constructors
                // may retain only the bare public slot.  A qualified slot that
                // still equals its bare mirror carries no additional value
                // information; collapse just those mirrors so the two
                // construction paths compare the same.  Keep role-owned slots
                // and qualified slots with different values: they are observable
                // through the declaring class and must remain part of eqv.
                let mut visible_entries = |map: &AttrMap| {
                    map.iter()
                        .filter(|(key, value)| {
                            !is_private_attribute(key, private.as_deref())
                                && !is_redundant_qualified_attribute(map, key, value, seen)
                        })
                        .map(|(key, value)| (*key, value.clone()))
                        .collect::<Vec<_>>()
                };
                let a_entries = visible_entries(&a_map);
                let b_entries = visible_entries(&b_map);
                a_entries.len() == b_entries.len()
                    && a_entries.iter().all(|(key, value)| {
                        b_entries
                            .iter()
                            .find(|(other_key, _)| other_key == key)
                            .is_some_and(|(_, other)| value.eqv_inner(other, seen))
                    })
            }
            // The Nil value IS the Nil type object: a `Package("Nil")` obtained
            // via type lookup (`::('Nil')`) denotes the same singleton.
            (ValueView::Nil, ValueView::Package(name))
            | (ValueView::Package(name), ValueView::Nil)
                if name == "Nil" =>
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
                inner.eqv_inner(other, seen)
            }
            (_, ValueView::Mixin(inner, mix))
                if mix.len() == 1 && mix.contains_key("__mutsu_topic_ro__") =>
            {
                self.eqv_inner(inner, seen)
            }
            // Mixin (allomorphs): compare both base values and mixin maps with eqv
            (ValueView::Mixin(a, a_mix), ValueView::Mixin(b, b_mix)) => {
                // A punned role (`R.new`) is a mixin over an instance of the
                // role, its attribute values in the mixin map. When a user
                // `raku` decides the pair, those values are part of what it
                // rendered, so only the composition itself is compared below.
                // Likewise a private role attribute is invisible to the
                // default `.raku`, so its mixin-map slot is skipped.
                let mode = match (a.view(), b.view()) {
                    (
                        ValueView::Instance { class_name: ca, .. },
                        ValueView::Instance { class_name: cb, .. },
                    ) if ca == cb => seen.hook.instance_eqv(a, b),
                    _ => InstanceEqv::Structural,
                };
                let (rendered, private) = match mode {
                    InstanceEqv::Rendered(answer) => (Some(answer), None),
                    InstanceEqv::Public(private) => (None, Some(private)),
                    InstanceEqv::Decided(_) | InstanceEqv::Structural => (None, None),
                };
                match rendered {
                    Some(false) => return false,
                    Some(true) => {}
                    None if !a.eqv_inner(b, seen) => return false,
                    None => {}
                }
                let attr_prefix = crate::runtime::meta_ns::MetaNs::Attr.prefix();
                // Compare mixin maps (e.g. Str part of allomorphs), ignoring the
                // `__mutsu_role_seq__` application-order and
                // `__mutsu_role_group__` application-grouping bookkeeping
                // entries (todo/tickets/mixin-role-order-not-tracked.md,
                // closed): two separately-built `X but Role` values with
                // otherwise identical composition are `eqv` regardless of which
                // process-global instant each was stamped at. The ORDER and
                // GROUPING those stamps encode are compared where they belong,
                // by `mixin_identity_key` for `===` and `mixin_composition_key`
                // for `.WHAT`; raw stamp values never match across two builds.
                let is_role_seq = |k: &str| {
                    k.starts_with("__mutsu_role_seq__")
                        || k.starts_with("__mutsu_role_group__")
                        || k.strip_prefix(attr_prefix).is_some_and(|name| {
                            rendered.is_some()
                                || private
                                    .as_deref()
                                    .is_some_and(|p| p.iter().any(|n| n == name))
                        })
                };
                let a_relevant = a_mix.iter().filter(|(k, _)| !is_role_seq(k));
                let a_count = a_mix.keys().filter(|k| !is_role_seq(k)).count();
                let b_count = b_mix.keys().filter(|k| !is_role_seq(k)).count();
                if a_count != b_count {
                    return false;
                }
                a_relevant
                    .into_iter()
                    .all(|(k, v)| b_mix.get(k).is_some_and(|bv| v.eqv_inner(bv, seen)))
            }
            // Cross-type comparisons always return false for eqv
            _ => false,
        }
    }
}

/// Whether an instance slot holds one of `private`'s attribute names (bare or
/// class- or role-qualified `Owner\0name`).
fn is_private_attribute(key: &Symbol, private: Option<&[String]>) -> bool {
    let Some(private) = private else {
        return false;
    };
    let key = key.resolve();
    let bare = key.rsplit_once('\0').map_or(key.as_str(), |(_, bare)| bare);
    private.iter().any(|name| name == bare)
}

/// Whether a qualified instance slot is merely a copy of the corresponding
/// bare attribute: a constructor-generated mirror of an inherited or
/// redeclared attribute, or a role-qualified mirror a punned role's method
/// call leaves behind. A role slot reads back through its bare name when it
/// is absent (`MixinOverrides::role_attribute`), so one equal to the bare slot
/// carries no information; one with a different value stays part of eqv.
fn is_redundant_qualified_attribute(
    map: &AttrMap,
    key: &Symbol,
    value: &Value,
    seen: &mut EqvCtx<'_>,
) -> bool {
    let key = key.resolve();
    let Some((owner, bare_name)) = key.rsplit_once('\0') else {
        return false;
    };
    if owner.is_empty() {
        return false;
    }
    map.get(bare_name)
        .is_some_and(|bare_value| value.eqv_inner(bare_value, seen))
}
