use super::*;

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        let match_equals_pair_array =
            |attrs: &crate::gc::Gc<InstanceAttrs>, arr: &crate::gc::Gc<ArrayData>| {
                if arr.len() != 2 {
                    return false;
                }
                let map = attrs.as_map();
                let (Some(from), Some(matched)) = (map.get("from"), map.get("str")) else {
                    return false;
                };
                arr[0] == *from && arr[1] == *matched
            };
        match (self.view(), other.view()) {
            (ValueView::Int(a), ValueView::Int(b)) => a == b,
            (ValueView::BigInt(a), ValueView::BigInt(b)) => a == b,
            (ValueView::BigInt(a), ValueView::Int(b))
            | (ValueView::Int(b), ValueView::BigInt(a)) => **a == NumBigInt::from(b),
            (ValueView::Num(a), ValueView::Num(b)) => (a.is_nan() && b.is_nan()) || a == b,
            (ValueView::Int(a), ValueView::Num(b)) => (a as f64) == b,
            (ValueView::Num(a), ValueView::Int(b)) => a == (b as f64),
            (ValueView::Str(a), ValueView::Str(b)) => a == b,
            (ValueView::Bool(a), ValueView::Bool(b)) => a == b,
            (ValueView::Range(a1, b1), ValueView::Range(a2, b2)) => a1 == a2 && b1 == b2,
            (ValueView::RangeExcl(a1, b1), ValueView::RangeExcl(a2, b2)) => a1 == a2 && b1 == b2,
            (ValueView::RangeExclStart(a1, b1), ValueView::RangeExclStart(a2, b2)) => {
                a1 == a2 && b1 == b2
            }
            (ValueView::RangeExclBoth(a1, b1), ValueView::RangeExclBoth(a2, b2)) => {
                a1 == a2 && b1 == b2
            }
            (
                ValueView::GenericRange {
                    start: s1,
                    end: e1,
                    excl_start: es1,
                    excl_end: ee1,
                },
                ValueView::GenericRange {
                    start: s2,
                    end: e2,
                    excl_start: es2,
                    excl_end: ee2,
                },
            ) => es1 == es2 && ee1 == ee2 && s1 == s2 && e1 == e2,
            (ValueView::Array(a, ..), ValueView::Array(b, ..)) => a == b,
            (ValueView::Seq(a), ValueView::Seq(b)) => a == b,
            (ValueView::Slip(a), ValueView::Slip(b)) => a == b,
            (ValueView::Array(a, ..), ValueView::Seq(b))
            | (ValueView::Seq(b), ValueView::Array(a, ..))
            | (ValueView::Array(a, ..), ValueView::Slip(b))
            | (ValueView::Slip(b), ValueView::Array(a, ..)) => a.items == **b,
            (ValueView::Seq(a), ValueView::Slip(b)) | (ValueView::Slip(b), ValueView::Seq(a)) => {
                a.as_ref() == b.as_ref()
            }
            (ValueView::Hash(a), ValueView::Hash(b)) => a == b,
            (ValueView::Rat(a1, b1), ValueView::Rat(a2, b2)) => {
                if b1 == 0 && b2 == 0 && a1 == 0 && a2 == 0 {
                    return false; // NaN != NaN
                }
                a1 == a2 && b1 == b2
            }
            (ValueView::Rat(n, d), ValueView::Int(i))
            | (ValueView::Int(i), ValueView::Rat(n, d)) => d != 0 && n == i * d,
            (ValueView::Rat(n, d), ValueView::Num(f))
            | (ValueView::Num(f), ValueView::Rat(n, d)) => {
                if d == 0 {
                    return false;
                }
                (n as f64 / d as f64) == f
            }
            (ValueView::BigRat(an, ad), ValueView::BigRat(bn, bd)) => an == bn && ad == bd,
            (ValueView::BigRat(n, d), ValueView::Int(i))
            | (ValueView::Int(i), ValueView::BigRat(n, d)) => {
                !d.is_zero() && *n == NumBigInt::from(i) * d
            }
            (ValueView::BigRat(n, d), ValueView::Rat(rn, rd))
            | (ValueView::Rat(rn, rd), ValueView::BigRat(n, d)) => {
                !d.is_zero()
                    && rd != 0
                    && n.clone() * NumBigInt::from(rd) == NumBigInt::from(rn) * d
            }
            (ValueView::BigRat(n, d), ValueView::Num(f))
            | (ValueView::Num(f), ValueView::BigRat(n, d)) => {
                if d.is_zero() {
                    return false;
                }
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)) == f
            }
            (ValueView::Complex(r1, i1), ValueView::Complex(r2, i2)) => {
                let f_eq = |a: f64, b: f64| (a.is_nan() && b.is_nan()) || a == b;
                f_eq(r1, r2) && f_eq(i1, i2)
            }
            (ValueView::Complex(r, i), ValueView::Int(n))
            | (ValueView::Int(n), ValueView::Complex(r, i)) => i == 0.0 && r == n as f64,
            (ValueView::Complex(r, i), ValueView::Num(f))
            | (ValueView::Num(f), ValueView::Complex(r, i)) => i == 0.0 && r == f,
            (ValueView::Complex(r, i), ValueView::Rat(n, d))
            | (ValueView::Rat(n, d), ValueView::Complex(r, i)) => {
                d != 0 && i == 0.0 && r == (n as f64 / d as f64)
            }
            (ValueView::Complex(r, i), ValueView::FatRat(n, d))
            | (ValueView::FatRat(n, d), ValueView::Complex(r, i)) => {
                d != 0 && i == 0.0 && r == (n as f64 / d as f64)
            }
            (ValueView::Complex(r, i), ValueView::BigRat(n, d))
            | (ValueView::BigRat(n, d), ValueView::Complex(r, i)) => {
                !d.is_zero()
                    && i == 0.0
                    && r == (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
            }
            (ValueView::FatRat(a1, b1), ValueView::FatRat(a2, b2)) => a1 == a2 && b1 == b2,
            (ValueView::Set(a, _), ValueView::Set(b, _)) => a == b,
            (ValueView::Bag(a, _), ValueView::Bag(b, _)) => a == b,
            (ValueView::Mix(a, _), ValueView::Mix(b, _)) => a == b,
            (
                ValueView::CompUnitDepSpec { short_name: a },
                ValueView::CompUnitDepSpec { short_name: b },
            ) => a == b,
            (ValueView::Package(a), ValueView::Package(b)) => a == b,
            (ValueView::Pair(ak, av), ValueView::Pair(bk, bv)) => ak == bk && av == bv,
            (ValueView::ValuePair(ak, av), ValueView::ValuePair(bk, bv)) => ak == bk && av == bv,
            (ValueView::Pair(ak, av), ValueView::ValuePair(bk, bv)) => {
                matches!(bk.view(), ValueView::Str(s) if s.as_str() == ak) && av == bv
            }
            (ValueView::ValuePair(ak, av), ValueView::Pair(bk, bv)) => {
                matches!(ak.view(), ValueView::Str(s) if s.as_str() == bk) && av == bv
            }
            (
                ValueView::Enum {
                    enum_type: at,
                    key: ak,
                    ..
                },
                ValueView::Enum {
                    enum_type: bt,
                    key: bk,
                    ..
                },
            ) => at == bt && ak == bk,
            (ValueView::Regex(a), ValueView::Regex(b)) => a == b,
            (ValueView::RegexWithAdverbs(a), ValueView::RegexWithAdverbs(b)) => {
                a.pattern == b.pattern
                    && a.global == b.global
                    && a.exhaustive == b.exhaustive
                    && a.overlap == b.overlap
                    && a.repeat == b.repeat
                    && a.nth == b.nth
                    && a.perl5 == b.perl5
                    && a.pos == b.pos
                    && a.continue_ == b.continue_
                    && a.ignore_case == b.ignore_case
                    && a.sigspace == b.sigspace
                    && a.samecase == b.samecase
                    && a.samespace == b.samespace
            }
            (
                ValueView::Routine {
                    package: ap,
                    name: an,
                    ..
                },
                ValueView::Routine {
                    package: bp,
                    name: bn,
                    ..
                },
            ) => ap == bp && an == bn,
            // `IO::Handle` carries internal open-state attributes (`handle` — an
            // opaque integer indexing this process's open-file registry; `mode`
            // — how it happened to be opened) that are unique per `.open()` call
            // and have no bearing on logical identity. Verified against real
            // Rakudo (2022.12): `$cat1 eqv $cat2` for two `IO::CatHandle`s built
            // from independently-opened handles to the same file (one opened
            // directly, one reopened via `$h.raku.EVAL` reconstruction, as
            // `IO::CatHandle.new` does internally for an un-opened source) is
            // `True` — so Rakudo's own comparison only looks at the public,
            // reproducible attributes (path/chomp/nl-in/nl-out/encoding/bin),
            // never at internal open state. Compare that allow-list only
            // (mirrors the class-specific carve-outs for Date/DateTime/Signature
            // in `Value::eqv`).
            (
                ValueView::Instance {
                    class_name: a,
                    attributes: aa,
                    ..
                },
                ValueView::Instance {
                    class_name: b,
                    attributes: ba,
                    ..
                },
            ) if a == "IO::Handle" && b == "IO::Handle" => {
                let a_map = aa.as_map();
                let b_map = ba.as_map();
                const COMPARABLE_KEYS: &[&str] =
                    &["path", "chomp", "nl-in", "nl-out", "encoding", "bin"];
                COMPARABLE_KEYS
                    .iter()
                    .all(|k| a_map.get(*k) == b_map.get(*k))
            }
            (
                ValueView::Instance {
                    class_name: a,
                    attributes: aa,
                    ..
                },
                ValueView::Instance {
                    class_name: b,
                    attributes: ba,
                    ..
                },
            ) => a == b && aa == ba,
            (
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                },
                ValueView::Array(items, ..),
            ) if class_name == "Match" => match_equals_pair_array(attributes, items),
            (
                ValueView::Array(items, ..),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                },
            ) if class_name == "Match" => match_equals_pair_array(attributes, items),
            (
                ValueView::Junction {
                    kind: ak,
                    values: av,
                },
                ValueView::Junction {
                    kind: bk,
                    values: bv,
                },
            ) => ak == bk && av == bv,
            (ValueView::Sub(a), ValueView::Sub(b)) => a.id == b.id,
            (ValueView::LazyList(a), ValueView::LazyList(b)) => crate::gc::Gc::ptr_eq(a, b),
            (
                ValueView::Version {
                    parts: ap,
                    plus: aplus,
                    minus: aminus,
                },
                ValueView::Version {
                    parts: bp,
                    plus: bplus,
                    minus: bminus,
                },
            ) => {
                if aplus != bplus || aminus != bminus {
                    return false;
                }
                // Normalize trailing zeroes: compare after stripping trailing Num(0)
                let a_norm = Self::version_strip_trailing_zeros(ap);
                let b_norm = Self::version_strip_trailing_zeros(bp);
                a_norm == b_norm
            }
            (ValueView::Promise(a), ValueView::Promise(b)) => a == b,
            (ValueView::Channel(a), ValueView::Channel(b)) => a == b,
            (ValueView::Nil, ValueView::Nil) => true,
            (
                ValueView::Capture {
                    positional: ap,
                    named: an,
                },
                ValueView::Capture {
                    positional: bp,
                    named: bn,
                },
            ) => ap == bp && an == bn,
            // Mixin (allomorphic types): compare inner values and mixin maps
            (ValueView::Mixin(a_inner, a_mix), ValueView::Mixin(b_inner, b_mix)) => {
                a_inner == b_inner && a_mix == b_mix
            }
            // Mixin vs non-Mixin: delegate to the inner value
            (ValueView::Mixin(inner, _), _) => inner.as_ref() == other,
            (_, ValueView::Mixin(inner, _)) => self == inner.as_ref(),
            // LazyThunk: compare cached values if available. Check Arc pointer
            // identity first — the same thunk compared to itself must short-circuit
            // before locking, since `a.cache` and `b.cache` are the *same* mutex
            // and a non-reentrant std `Mutex` would deadlock on the second lock.
            // (Hit by the END-phaser overlay's `v != orig_v` on a lexical bound to
            // a `lazy { … }` thunk: captured and live env hold the same Arc.)
            (ValueView::LazyThunk(a), ValueView::LazyThunk(b)) => {
                if Arc::ptr_eq(a, b) {
                    return true;
                }
                let a_cache = a.cache.lock().unwrap();
                let b_cache = b.cache.lock().unwrap();
                match (a_cache.as_ref(), b_cache.as_ref()) {
                    (Some(av), Some(bv)) => av == bv,
                    _ => false,
                }
            }
            (ValueView::LazyThunk(thunk), _) => {
                let cache = thunk.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    cached == other
                } else {
                    false
                }
            }
            (_, ValueView::LazyThunk(thunk)) => {
                let cache = thunk.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    self == cached
                } else {
                    false
                }
            }
            (ValueView::Uni(a), ValueView::Uni(b)) => a.form == b.form && a.text == b.text,
            // ContainerRef: deref and compare inner values.
            // Check Arc pointer identity first to avoid deadlock on same-Arc comparison.
            (ValueView::ContainerRef(a), ValueView::ContainerRef(b)) => {
                if crate::gc::Gc::ptr_eq(a, b) {
                    return true;
                }
                let a_val = a.lock().unwrap().clone();
                let b_val = b.lock().unwrap().clone();
                a_val == b_val
            }
            (ValueView::ContainerRef(a), _) => {
                let a_val = a.lock().unwrap().clone();
                &a_val == other
            }
            (_, ValueView::ContainerRef(a)) => {
                let a_val = a.lock().unwrap().clone();
                self == &a_val
            }
            _ => false,
        }
    }
}
