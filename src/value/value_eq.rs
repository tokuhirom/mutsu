use super::*;

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        let match_equals_pair_array = |attrs: &Arc<InstanceAttrs>, arr: &Arc<ArrayData>| {
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
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::BigInt(a), Value::BigInt(b)) => a == b,
            (Value::BigInt(a), Value::Int(b)) | (Value::Int(b), Value::BigInt(a)) => {
                **a == NumBigInt::from(*b)
            }
            (Value::Num(a), Value::Num(b)) => (a.is_nan() && b.is_nan()) || a == b,
            (Value::Int(a), Value::Num(b)) => (*a as f64) == *b,
            (Value::Num(a), Value::Int(b)) => *a == (*b as f64),
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Range(a1, b1), Value::Range(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::RangeExcl(a1, b1), Value::RangeExcl(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::RangeExclStart(a1, b1), Value::RangeExclStart(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::RangeExclBoth(a1, b1), Value::RangeExclBoth(a2, b2)) => a1 == a2 && b1 == b2,
            (
                Value::GenericRange {
                    start: s1,
                    end: e1,
                    excl_start: es1,
                    excl_end: ee1,
                },
                Value::GenericRange {
                    start: s2,
                    end: e2,
                    excl_start: es2,
                    excl_end: ee2,
                },
            ) => es1 == es2 && ee1 == ee2 && s1 == s2 && e1 == e2,
            (Value::Array(a, ..), Value::Array(b, ..)) => a == b,
            (Value::Seq(a), Value::Seq(b)) => a == b,
            (Value::Slip(a), Value::Slip(b)) => a == b,
            (Value::Array(a, ..), Value::Seq(b))
            | (Value::Seq(b), Value::Array(a, ..))
            | (Value::Array(a, ..), Value::Slip(b))
            | (Value::Slip(b), Value::Array(a, ..)) => a.items == **b,
            (Value::Seq(a), Value::Slip(b)) | (Value::Slip(b), Value::Seq(a)) => {
                a.as_ref() == b.as_ref()
            }
            (Value::Hash(a), Value::Hash(b)) => a == b,
            (Value::Rat(a1, b1), Value::Rat(a2, b2)) => {
                if *b1 == 0 && *b2 == 0 && *a1 == 0 && *a2 == 0 {
                    return false; // NaN != NaN
                }
                a1 == a2 && b1 == b2
            }
            (Value::Rat(n, d), Value::Int(i)) | (Value::Int(i), Value::Rat(n, d)) => {
                *d != 0 && *n == *i * *d
            }
            (Value::Rat(n, d), Value::Num(f)) | (Value::Num(f), Value::Rat(n, d)) => {
                if *d == 0 {
                    return false;
                }
                (*n as f64 / *d as f64) == *f
            }
            (Value::BigRat(an, ad), Value::BigRat(bn, bd)) => an == bn && ad == bd,
            (Value::BigRat(n, d), Value::Int(i)) | (Value::Int(i), Value::BigRat(n, d)) => {
                let (n, d) = (n.as_ref(), d.as_ref());
                !d.is_zero() && *n == NumBigInt::from(*i) * d
            }
            (Value::BigRat(n, d), Value::Rat(rn, rd))
            | (Value::Rat(rn, rd), Value::BigRat(n, d)) => {
                let (n, d) = (n.as_ref(), d.as_ref());
                !d.is_zero()
                    && *rd != 0
                    && n.clone() * NumBigInt::from(*rd) == NumBigInt::from(*rn) * d
            }
            (Value::BigRat(n, d), Value::Num(f)) | (Value::Num(f), Value::BigRat(n, d)) => {
                let (n, d) = (n.as_ref(), d.as_ref());
                if d.is_zero() {
                    return false;
                }
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)) == *f
            }
            (Value::Complex(r1, i1), Value::Complex(r2, i2)) => {
                let f_eq = |a: &f64, b: &f64| (a.is_nan() && b.is_nan()) || a == b;
                f_eq(r1, r2) && f_eq(i1, i2)
            }
            (Value::Complex(r, i), Value::Int(n)) | (Value::Int(n), Value::Complex(r, i)) => {
                *i == 0.0 && *r == *n as f64
            }
            (Value::Complex(r, i), Value::Num(f)) | (Value::Num(f), Value::Complex(r, i)) => {
                *i == 0.0 && *r == *f
            }
            (Value::Complex(r, i), Value::Rat(n, d)) | (Value::Rat(n, d), Value::Complex(r, i)) => {
                *d != 0 && *i == 0.0 && *r == (*n as f64 / *d as f64)
            }
            (Value::Complex(r, i), Value::FatRat(n, d))
            | (Value::FatRat(n, d), Value::Complex(r, i)) => {
                *d != 0 && *i == 0.0 && *r == (*n as f64 / *d as f64)
            }
            (Value::Complex(r, i), Value::BigRat(n, d))
            | (Value::BigRat(n, d), Value::Complex(r, i)) => {
                let (n, d) = (n.as_ref(), d.as_ref());
                !d.is_zero()
                    && *i == 0.0
                    && *r == (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
            }
            (Value::FatRat(a1, b1), Value::FatRat(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::Set(a, _), Value::Set(b, _)) => a == b,
            (Value::Bag(a, _), Value::Bag(b, _)) => a == b,
            (Value::Mix(a, _), Value::Mix(b, _)) => a == b,
            (
                Value::CompUnitDepSpec { short_name: a },
                Value::CompUnitDepSpec { short_name: b },
            ) => a == b,
            (Value::Package(a), Value::Package(b)) => a == b,
            (Value::Pair(ak, av), Value::Pair(bk, bv)) => ak == bk && av == bv,
            (Value::ValuePair(ak, av), Value::ValuePair(bk, bv)) => ak == bk && av == bv,
            (Value::Pair(ak, av), Value::ValuePair(bk, bv)) => {
                matches!(bk.as_ref(), Value::Str(s) if s.as_str() == ak) && av == bv
            }
            (Value::ValuePair(ak, av), Value::Pair(bk, bv)) => {
                matches!(ak.as_ref(), Value::Str(s) if s.as_str() == bk) && av == bv
            }
            (
                Value::Enum {
                    enum_type: at,
                    key: ak,
                    ..
                },
                Value::Enum {
                    enum_type: bt,
                    key: bk,
                    ..
                },
            ) => at == bt && ak == bk,
            (Value::Regex(a), Value::Regex(b)) => a == b,
            (Value::RegexWithAdverbs(a), Value::RegexWithAdverbs(b)) => {
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
                Value::Routine {
                    package: ap,
                    name: an,
                    ..
                },
                Value::Routine {
                    package: bp,
                    name: bn,
                    ..
                },
            ) => ap == bp && an == bn,
            (
                Value::Instance {
                    class_name: a,
                    attributes: aa,
                    ..
                },
                Value::Instance {
                    class_name: b,
                    attributes: ba,
                    ..
                },
            ) => a == b && aa == ba,
            (
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                },
                Value::Array(items, ..),
            ) if class_name == "Match" => match_equals_pair_array(attributes, items),
            (
                Value::Array(items, ..),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                },
            ) if class_name == "Match" => match_equals_pair_array(attributes, items),
            (
                Value::Junction {
                    kind: ak,
                    values: av,
                },
                Value::Junction {
                    kind: bk,
                    values: bv,
                },
            ) => ak == bk && av == bv,
            (Value::Sub(a), Value::Sub(b)) => a.id == b.id,
            (Value::LazyList(a), Value::LazyList(b)) => Arc::ptr_eq(a, b),
            (
                Value::Version {
                    parts: ap,
                    plus: aplus,
                    minus: aminus,
                },
                Value::Version {
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
            (Value::Promise(a), Value::Promise(b)) => a == b,
            (Value::Channel(a), Value::Channel(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (
                Value::Capture {
                    positional: ap,
                    named: an,
                },
                Value::Capture {
                    positional: bp,
                    named: bn,
                },
            ) => ap == bp && an == bn,
            // Mixin (allomorphic types): compare inner values and mixin maps
            (Value::Mixin(a_inner, a_mix), Value::Mixin(b_inner, b_mix)) => {
                a_inner == b_inner && a_mix == b_mix
            }
            // Mixin vs non-Mixin: delegate to the inner value
            (Value::Mixin(inner, _), other) | (other, Value::Mixin(inner, _)) => {
                inner.as_ref() == other
            }
            // LazyThunk: compare cached values if available. Check Arc pointer
            // identity first — the same thunk compared to itself must short-circuit
            // before locking, since `a.cache` and `b.cache` are the *same* mutex
            // and a non-reentrant std `Mutex` would deadlock on the second lock.
            // (Hit by the END-phaser overlay's `v != orig_v` on a lexical bound to
            // a `lazy { … }` thunk: captured and live env hold the same Arc.)
            (Value::LazyThunk(a), Value::LazyThunk(b)) => {
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
            (Value::LazyThunk(thunk), other) | (other, Value::LazyThunk(thunk)) => {
                let cache = thunk.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    cached == other
                } else {
                    false
                }
            }
            (Value::Uni(a), Value::Uni(b)) => a.form == b.form && a.text == b.text,
            // ContainerRef: deref and compare inner values.
            // Check Arc pointer identity first to avoid deadlock on same-Arc comparison.
            (Value::ContainerRef(a), Value::ContainerRef(b)) => {
                if Arc::ptr_eq(a, b) {
                    return true;
                }
                let a_val = a.lock().unwrap().clone();
                let b_val = b.lock().unwrap().clone();
                a_val == b_val
            }
            (Value::ContainerRef(a), other) | (other, Value::ContainerRef(a)) => {
                let a_val = a.lock().unwrap().clone();
                a_val == *other
            }
            _ => false,
        }
    }
}
