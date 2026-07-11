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
        match (self, other) {
            (Value(ValueRepr::Int(a)), Value(ValueRepr::Int(b))) => a == b,
            (Value(ValueRepr::BigInt(a)), Value(ValueRepr::BigInt(b))) => a == b,
            (Value(ValueRepr::BigInt(a)), Value(ValueRepr::Int(b)))
            | (Value(ValueRepr::Int(b)), Value(ValueRepr::BigInt(a))) => **a == NumBigInt::from(*b),
            (Value(ValueRepr::Num(a)), Value(ValueRepr::Num(b))) => {
                (a.is_nan() && b.is_nan()) || a == b
            }
            (Value(ValueRepr::Int(a)), Value(ValueRepr::Num(b))) => (*a as f64) == *b,
            (Value(ValueRepr::Num(a)), Value(ValueRepr::Int(b))) => *a == (*b as f64),
            (Value(ValueRepr::Str(a)), Value(ValueRepr::Str(b))) => a == b,
            (Value(ValueRepr::Bool(a)), Value(ValueRepr::Bool(b))) => a == b,
            (Value(ValueRepr::Range(a1, b1)), Value(ValueRepr::Range(a2, b2))) => {
                a1 == a2 && b1 == b2
            }
            (Value(ValueRepr::RangeExcl(a1, b1)), Value(ValueRepr::RangeExcl(a2, b2))) => {
                a1 == a2 && b1 == b2
            }
            (
                Value(ValueRepr::RangeExclStart(a1, b1)),
                Value(ValueRepr::RangeExclStart(a2, b2)),
            ) => a1 == a2 && b1 == b2,
            (Value(ValueRepr::RangeExclBoth(a1, b1)), Value(ValueRepr::RangeExclBoth(a2, b2))) => {
                a1 == a2 && b1 == b2
            }
            (
                Value(ValueRepr::GenericRange {
                    start: s1,
                    end: e1,
                    excl_start: es1,
                    excl_end: ee1,
                }),
                Value(ValueRepr::GenericRange {
                    start: s2,
                    end: e2,
                    excl_start: es2,
                    excl_end: ee2,
                }),
            ) => es1 == es2 && ee1 == ee2 && s1 == s2 && e1 == e2,
            (Value(ValueRepr::Array(a, ..)), Value(ValueRepr::Array(b, ..))) => a == b,
            (Value(ValueRepr::Seq(a)), Value(ValueRepr::Seq(b))) => a == b,
            (Value(ValueRepr::Slip(a)), Value(ValueRepr::Slip(b))) => a == b,
            (Value(ValueRepr::Array(a, ..)), Value(ValueRepr::Seq(b)))
            | (Value(ValueRepr::Seq(b)), Value(ValueRepr::Array(a, ..)))
            | (Value(ValueRepr::Array(a, ..)), Value(ValueRepr::Slip(b)))
            | (Value(ValueRepr::Slip(b)), Value(ValueRepr::Array(a, ..))) => a.items == **b,
            (Value(ValueRepr::Seq(a)), Value(ValueRepr::Slip(b)))
            | (Value(ValueRepr::Slip(b)), Value(ValueRepr::Seq(a))) => a.as_ref() == b.as_ref(),
            (Value(ValueRepr::Hash(a, _)), Value(ValueRepr::Hash(b, _))) => a == b,
            (Value(ValueRepr::Rat(a1, b1)), Value(ValueRepr::Rat(a2, b2))) => {
                if *b1 == 0 && *b2 == 0 && *a1 == 0 && *a2 == 0 {
                    return false; // NaN != NaN
                }
                a1 == a2 && b1 == b2
            }
            (Value(ValueRepr::Rat(n, d)), Value(ValueRepr::Int(i)))
            | (Value(ValueRepr::Int(i)), Value(ValueRepr::Rat(n, d))) => *d != 0 && *n == *i * *d,
            (Value(ValueRepr::Rat(n, d)), Value(ValueRepr::Num(f)))
            | (Value(ValueRepr::Num(f)), Value(ValueRepr::Rat(n, d))) => {
                if *d == 0 {
                    return false;
                }
                (*n as f64 / *d as f64) == *f
            }
            (Value(ValueRepr::BigRat(an, ad)), Value(ValueRepr::BigRat(bn, bd))) => {
                an == bn && ad == bd
            }
            (Value(ValueRepr::BigRat(n, d)), Value(ValueRepr::Int(i)))
            | (Value(ValueRepr::Int(i)), Value(ValueRepr::BigRat(n, d))) => {
                let (n, d) = (n.as_ref(), d.as_ref());
                !d.is_zero() && *n == NumBigInt::from(*i) * d
            }
            (Value(ValueRepr::BigRat(n, d)), Value(ValueRepr::Rat(rn, rd)))
            | (Value(ValueRepr::Rat(rn, rd)), Value(ValueRepr::BigRat(n, d))) => {
                let (n, d) = (n.as_ref(), d.as_ref());
                !d.is_zero()
                    && *rd != 0
                    && n.clone() * NumBigInt::from(*rd) == NumBigInt::from(*rn) * d
            }
            (Value(ValueRepr::BigRat(n, d)), Value(ValueRepr::Num(f)))
            | (Value(ValueRepr::Num(f)), Value(ValueRepr::BigRat(n, d))) => {
                let (n, d) = (n.as_ref(), d.as_ref());
                if d.is_zero() {
                    return false;
                }
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)) == *f
            }
            (Value(ValueRepr::Complex(r1, i1)), Value(ValueRepr::Complex(r2, i2))) => {
                let f_eq = |a: &f64, b: &f64| (a.is_nan() && b.is_nan()) || a == b;
                f_eq(r1, r2) && f_eq(i1, i2)
            }
            (Value(ValueRepr::Complex(r, i)), Value(ValueRepr::Int(n)))
            | (Value(ValueRepr::Int(n)), Value(ValueRepr::Complex(r, i))) => {
                *i == 0.0 && *r == *n as f64
            }
            (Value(ValueRepr::Complex(r, i)), Value(ValueRepr::Num(f)))
            | (Value(ValueRepr::Num(f)), Value(ValueRepr::Complex(r, i))) => *i == 0.0 && *r == *f,
            (Value(ValueRepr::Complex(r, i)), Value(ValueRepr::Rat(n, d)))
            | (Value(ValueRepr::Rat(n, d)), Value(ValueRepr::Complex(r, i))) => {
                *d != 0 && *i == 0.0 && *r == (*n as f64 / *d as f64)
            }
            (Value(ValueRepr::Complex(r, i)), Value(ValueRepr::FatRat(n, d)))
            | (Value(ValueRepr::FatRat(n, d)), Value(ValueRepr::Complex(r, i))) => {
                *d != 0 && *i == 0.0 && *r == (*n as f64 / *d as f64)
            }
            (Value(ValueRepr::Complex(r, i)), Value(ValueRepr::BigRat(n, d)))
            | (Value(ValueRepr::BigRat(n, d)), Value(ValueRepr::Complex(r, i))) => {
                let (n, d) = (n.as_ref(), d.as_ref());
                !d.is_zero()
                    && *i == 0.0
                    && *r == (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
            }
            (Value(ValueRepr::FatRat(a1, b1)), Value(ValueRepr::FatRat(a2, b2))) => {
                a1 == a2 && b1 == b2
            }
            (Value(ValueRepr::Set(a, _)), Value(ValueRepr::Set(b, _))) => a == b,
            (Value(ValueRepr::Bag(a, _)), Value(ValueRepr::Bag(b, _))) => a == b,
            (Value(ValueRepr::Mix(a, _)), Value(ValueRepr::Mix(b, _))) => a == b,
            (
                Value(ValueRepr::CompUnitDepSpec { short_name: a }),
                Value(ValueRepr::CompUnitDepSpec { short_name: b }),
            ) => a == b,
            (Value(ValueRepr::Package(a)), Value(ValueRepr::Package(b))) => a == b,
            (Value(ValueRepr::Pair(ak, av)), Value(ValueRepr::Pair(bk, bv))) => {
                ak == bk && av == bv
            }
            (Value(ValueRepr::ValuePair(ak, av)), Value(ValueRepr::ValuePair(bk, bv))) => {
                ak == bk && av == bv
            }
            (Value(ValueRepr::Pair(ak, av)), Value(ValueRepr::ValuePair(bk, bv))) => {
                matches!(bk.as_ref(), Value(ValueRepr::Str(s)) if s.as_str() == ak) && av == bv
            }
            (Value(ValueRepr::ValuePair(ak, av)), Value(ValueRepr::Pair(bk, bv))) => {
                matches!(ak.as_ref(), Value(ValueRepr::Str(s)) if s.as_str() == bk) && av == bv
            }
            (
                Value(ValueRepr::Enum {
                    enum_type: at,
                    key: ak,
                    ..
                }),
                Value(ValueRepr::Enum {
                    enum_type: bt,
                    key: bk,
                    ..
                }),
            ) => at == bt && ak == bk,
            (Value(ValueRepr::Regex(a)), Value(ValueRepr::Regex(b))) => a == b,
            (Value(ValueRepr::RegexWithAdverbs(a)), Value(ValueRepr::RegexWithAdverbs(b))) => {
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
                Value(ValueRepr::Routine {
                    package: ap,
                    name: an,
                    ..
                }),
                Value(ValueRepr::Routine {
                    package: bp,
                    name: bn,
                    ..
                }),
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
                Value(ValueRepr::Instance {
                    class_name: a,
                    attributes: aa,
                    ..
                }),
                Value(ValueRepr::Instance {
                    class_name: b,
                    attributes: ba,
                    ..
                }),
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
                Value(ValueRepr::Instance {
                    class_name: a,
                    attributes: aa,
                    ..
                }),
                Value(ValueRepr::Instance {
                    class_name: b,
                    attributes: ba,
                    ..
                }),
            ) => a == b && aa == ba,
            (
                Value(ValueRepr::Instance {
                    class_name,
                    attributes,
                    ..
                }),
                Value(ValueRepr::Array(items, ..)),
            ) if class_name == "Match" => match_equals_pair_array(attributes, items),
            (
                Value(ValueRepr::Array(items, ..)),
                Value(ValueRepr::Instance {
                    class_name,
                    attributes,
                    ..
                }),
            ) if class_name == "Match" => match_equals_pair_array(attributes, items),
            (
                Value(ValueRepr::Junction {
                    kind: ak,
                    values: av,
                }),
                Value(ValueRepr::Junction {
                    kind: bk,
                    values: bv,
                }),
            ) => ak == bk && av == bv,
            (Value(ValueRepr::Sub(a)), Value(ValueRepr::Sub(b))) => a.id == b.id,
            (Value(ValueRepr::LazyList(a)), Value(ValueRepr::LazyList(b))) => {
                crate::gc::Gc::ptr_eq(a, b)
            }
            (
                Value(ValueRepr::Version {
                    parts: ap,
                    plus: aplus,
                    minus: aminus,
                }),
                Value(ValueRepr::Version {
                    parts: bp,
                    plus: bplus,
                    minus: bminus,
                }),
            ) => {
                if aplus != bplus || aminus != bminus {
                    return false;
                }
                // Normalize trailing zeroes: compare after stripping trailing Num(0)
                let a_norm = Self::version_strip_trailing_zeros(ap);
                let b_norm = Self::version_strip_trailing_zeros(bp);
                a_norm == b_norm
            }
            (Value(ValueRepr::Promise(a)), Value(ValueRepr::Promise(b))) => a == b,
            (Value(ValueRepr::Channel(a)), Value(ValueRepr::Channel(b))) => a == b,
            (Value(ValueRepr::Nil), Value(ValueRepr::Nil)) => true,
            (
                Value(ValueRepr::Capture {
                    positional: ap,
                    named: an,
                }),
                Value(ValueRepr::Capture {
                    positional: bp,
                    named: bn,
                }),
            ) => ap == bp && an == bn,
            // Mixin (allomorphic types): compare inner values and mixin maps
            (Value(ValueRepr::Mixin(a_inner, a_mix)), Value(ValueRepr::Mixin(b_inner, b_mix))) => {
                a_inner == b_inner && a_mix == b_mix
            }
            // Mixin vs non-Mixin: delegate to the inner value
            (Value(ValueRepr::Mixin(inner, _)), other)
            | (other, Value(ValueRepr::Mixin(inner, _))) => inner.as_ref() == other,
            // LazyThunk: compare cached values if available. Check Arc pointer
            // identity first — the same thunk compared to itself must short-circuit
            // before locking, since `a.cache` and `b.cache` are the *same* mutex
            // and a non-reentrant std `Mutex` would deadlock on the second lock.
            // (Hit by the END-phaser overlay's `v != orig_v` on a lexical bound to
            // a `lazy { … }` thunk: captured and live env hold the same Arc.)
            (Value(ValueRepr::LazyThunk(a)), Value(ValueRepr::LazyThunk(b))) => {
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
            (Value(ValueRepr::LazyThunk(thunk)), other)
            | (other, Value(ValueRepr::LazyThunk(thunk))) => {
                let cache = thunk.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    cached == other
                } else {
                    false
                }
            }
            (Value(ValueRepr::Uni(a)), Value(ValueRepr::Uni(b))) => {
                a.form == b.form && a.text == b.text
            }
            // ContainerRef: deref and compare inner values.
            // Check Arc pointer identity first to avoid deadlock on same-Arc comparison.
            (Value(ValueRepr::ContainerRef(a)), Value(ValueRepr::ContainerRef(b))) => {
                if crate::gc::Gc::ptr_eq(a, b) {
                    return true;
                }
                let a_val = a.lock().unwrap().clone();
                let b_val = b.lock().unwrap().clone();
                a_val == b_val
            }
            (Value(ValueRepr::ContainerRef(a)), other)
            | (other, Value(ValueRepr::ContainerRef(a))) => {
                let a_val = a.lock().unwrap().clone();
                a_val == *other
            }
            _ => false,
        }
    }
}
