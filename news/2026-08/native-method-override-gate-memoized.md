# Every native method call re-walked the receiver's MRO to ask "did anyone augment this?" — now memoized

Opened as `todo/perf/uniname-per-call-cost.md` ("`.uniname` costs ~7x raku per
call"), the last residue of the `uniname-sort` perf investigation. The ticket's
framing turned out to be wrong in a useful way: `.uniname` is not slow, and the
real finding is ~20% off **every** native method call in mutsu.

## The ticket's premise did not survive measurement

The ticket suspected `unicode_char_name_by_codepoint`'s per-call `String`
allocation. Benchmarking `.uniname` against other 0-arg methods on the same
receiver said otherwise (release, 131072-element `for` loop, us/elem):

| | mutsu | raku |
|---|---|---|
| loop floor | 0.30 | 0.02 |
| `.Str.chars` | 1.78 | 0.11 |
| `.base(16).chars` | 1.85 | 0.36 |
| `.uniname.chars` | 1.94 | 0.30 |

`.uniname.chars` costs only 0.16 us more than `.Str.chars`, and `.Str` returning
an existing string does no Unicode work at all. So the ~1.5 us was **not**
`.uniname` — it was the shared cost of dispatching two native methods, and
`.uniname`'s own lookup was already cheap. (In raku the relationship is the
opposite: `.uniname` costs 0.19 us more than `.Str`.)

## What the profile showed

A `perf` profile of the loop put ~7% of cycles in
`has_user_method` + `class_mro` + `Registry::user_method_overloads`, plus
`Symbol::intern`/`memcmp` churn feeding them. A `rust-gdb` breakpoint on
`has_user_method` (conditioned on the method name) named the caller in one shot:

```
#0  has_user_method (class_name="Int", method_name="uniname")
#1  native_lever_a_user_override        vm_call_method_compiled_cache.rs:183
#2  try_native_method_raw               vm_native_dispatch.rs:263
#3  exec_call_method_op_impl            vm_call_method_ops.rs:2022
```

`native_lever_a_user_override` is the gate that keeps a legal
`augment class Array { method sort {...} }` from being silently shadowed by the
native fast path. It sits on **every** native method call with a non-`Instance`
receiver, and it answered by walking the receiver's whole MRO
(`Int` → `Cool` → `Any` → `Mu`), asking `user_method_overloads` at each level —
every single call, purely to re-derive "no, nobody augmented `Int`".

## The fix

The answer is a pure function of the registry shape, so it is memoized on
`(receiver type name, method)` in `native_lever_a_override_cache`, following the
`multi_type_cacheable` pattern that already lives next to it: cleared by
`refresh_method_caches_for_generation` alongside every other method cache when
`registry().method_generation` bumps.

## Measured (release, median of 5, 131072 elements, us/elem)

| | before | after | gain |
|---|---|---|---|
| `.Str.chars` | 1.775 | 1.426 | **19.7%** |
| `.base(16).chars` | 1.845 | 1.473 | **20.2%** |
| `.uniname.chars` | 1.938 | 1.545 | **20.3%** |
| `.succ` | 1.168 | 0.964 | **17.5%** |
| `.abs` | 1.115 | 0.904 | **18.9%** |
| `map(*.succ)` | 0.964 | 0.749 | **22.3%** |

Consistently ~18-22% across every native method call, not just the one the
ticket named. Re-profiling afterwards shows the `has_user_method`/`class_mro`/
`user_method_overloads`/`intern` cluster gone from the top; what remains is
diffuse generic dispatch machinery (`view_kind`, `exec_set_local_op_inner`, the
`CallMethod` opcode handlers, thread-local access, malloc/free) with no single
dominant term left, so further gains here are a broader campaign rather than
another local fix.

## Soundness

The memo's one risk is an `augment` that lands *after* an entry is cached.
`t/native-override-cache-invalidation.t` (10 tests, validated against `raku`
first) exercises exactly that ordering — native call first to warm the memo,
then `EVAL`-time augment, then call again — for the same class (`Array.sort`),
for an MRO **ancestor** of the receiver (`List.first` seen through an `Array`
receiver, which is why the memo key must be the receiver type rather than the
declaring class), and for a second receiver type (`Range.sort`); plus three
checks that un-augmented methods still dispatch natively afterwards, i.e. the
memo is repopulated rather than poisoned.

The full `t/` suite (3502 files) and the **entire** roast whitelist (1436 files,
218,836 tests) pass locally — worth running in full here rather than a targeted
sweep, since the change sits on every method call in the language.
