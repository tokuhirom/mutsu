# Native `int`/`str`/`num` parameters reach the light-call fast paths (#8686 Phase 0)

`FastParamType::of` only recognized the boxed `Int`/`Str`/`Num`/`Bool`/`Rat` spellings, so a routine
whose signature used the lowercase native scalar types — the idiomatic `nqp::`-style used throughout
the ecosystem's own "fast" modules, above all the vendored `JSON::Fast` — always fell back to the
general, multi-dispatch-capable binder on every call, paying its full per-call cost even for a plain
`sub f(int $x, int $y) { ... }` with no traits, no `multi`, nothing the light paths could not otherwise
handle. #8686's own callgrind investigation measured this gap directly: a native-int-typed
two-parameter helper cost the same ~46,000 instructions per call as one with an `is rw` parameter,
confirming this classification gap alone was enough to disqualify an ordinary native-typed sub from
the fast paths, independent of `is rw`.

## What changed

`NativeInt`/`NativeStr`/`NativeNum` join the existing `FastParamType` tags. They are kept distinct
from `Int`/`Str`/`Num` rather than merged into them, because a bare type-object argument must be
**rejected** for a native slot but **accepted** for a boxed one: `sub f(int $x){}; f(int)` dies
"Cannot unbox a type object (int) to int" in both raku and mutsu, unlike `sub f(Int $x){}; f(Int)`,
which legitimately returns the `Int` type object
(`t/vm/frames/light-call-bare-type-accepts-type-object.t` pins the boxed case). Only the plain
full-width spellings (`int`/`str`/`num`) are covered, not the sized (`int8`/`uint32`/...) or C-width
alias forms — those stay on the general binder, matching the issue's own scope.

With the classification in place, the light and positional-light call paths now:

- accept `int`/`str`/`num` parameters at all, instead of excluding any signature that uses one;
- coerce a `Bool` argument to `Int` for `int` (`Bool` "does" `Int`, pinned by
  `t/nativecall/bool-native-int-unbox.t`), reusing `wrap_native_int_for_binding` — now `pub(crate)`
  instead of `pub(in crate::runtime)` — so the exact Bool-unbox/range-check/wrap logic the general
  binder applies is not re-derived, including its own out-of-range-`BigInt` overflow error
  ("Cannot unbox N bit wide bigint into native integer");
- register the same `__mutsu_type::` env metadata the general binder writes for a native parameter,
  so `~~ int`-style introspection inside the body still answers correctly
  (`t/nativecall/native-value-smartmatch.t` subtest 25).

`returns int`/`str`/`num` is intentionally left as a no-op check on the fast paths, matching the
general binder's own current *non*-enforcement of native return types (a `sub f() returns int { True
}` returns the unconverted `Bool`, not raku's coerced `Int`) — only parameter binding was in scope
for this slice.

## The metadata write would have eaten the whole point — gated on a constant-pool scan

The `~~ int` env-metadata write is not free: writing a fresh key into a light call's overlay env
forces a full env deep-copy, since the light path's whole design keeps env untouched (Arc-shared)
whenever nothing needs it. Measured via `MUTSU_VM_STATS=1`: 100,000 calls of
`sub f(int $x, int $y) { $x + $y }` went from 0 to 100,000 `env_deep_copies` when the write ran
unconditionally — exactly the overhead this whole change exists to avoid, and worse than doing nothing
for the overwhelming majority of native-typed hot loops (including `JSON::Fast`'s own helpers), which
never smartmatch or introspect their own native parameters.

The fix follows the same pattern `CompiledCode::reads_topic` already uses for the topic shadow write:
a `~~ int`/`.^name` probe against a native-typed local compiles the type name in as a `GetBareWord`
constant (verified with `--dump-bytecode`), so a body whose constant pool contains none of
`"int"`/`"str"`/`"num"` cannot possibly introspect a native parameter's declared type at all. A new
`CompiledCode::mentions_native_scalar_type_name` flag, computed once per compiled function in
`compute_needs_env_sync` (never per call), gates the write. A stray, unrelated string constant
happening to spell one of the three names only costs a harmless extra metadata write, exactly as a
stray `"_"` costs `reads_topic` one shadow write.

## Measured

Release build, wall-clock, 2,000,000 calls to `sub f(int $x, int $y) { $x + $y }`:

| | before (general binder) | after (light-call fast path) |
| --- | --- | --- |
| whole run | 11.28s | 1.87s (**~6x**) |

For comparison, the same call shape with boxed `Int $x, Int $y` params (already light-call eligible,
unaffected by this change) runs in ~1.65–2.0s on the same box — native-typed params are now
competitive with boxed ones, where before they were categorically excluded from the fast path.

`MUTSU_VM_STATS=1`, 100,000 calls, `env_deep_copies`:

| body | before this fix (unconditional write) | after (gated on `mentions_native_scalar_type_name`) |
| --- | --- | --- |
| `sub f(int $x, int $y) { $x + $y }` (no introspection) | 100,000 | 0 |
| `sub f(int $x) { $x ~~ int }` (introspects) | (already gated) | still writes correctly, `~~ int` still True |

`JSON::Fast`'s own headline workload (#8673) is **not** moved by this slice — its scan-position
helpers declare `int $pos is rw`, so every one of them stays on the general binder (`is_rw` remains
excluded from the light paths entirely). That is Phase 2 of #8686, which needs its own `Proposed` ADR
before any implementation attempt.

## A real regression caught by `make test`, fixed before landing

The first attempt at the `Mixin`/allomorph check compared an allomorph against its own **native**
`name_sym` (`"int"`/`"str"`/`"num"`), which never matches: `Value::isa_or_does_check`'s `my_type` is
always the boxed spelling (`"Int"`/`"Str"`/`"Num"`), never lowercase. That silently made every
`IntStr`/`NumStr`/... argument to a native `int`/`str`/`num` parameter fail its type check —
`make test` caught it as three real (not container-only) failures:
`t/concurrency/thread-lock/thread-clone-program-table-isolation.t` and both
`t/modules/batteries/{cro-http,http-deps}-battery.t`, all three going through
`MIME::Base64.encode-str`, whose `add-byte(str $x, ...)` helper is called with an `IntStr` literal.
Fixed by probing the boxed counterpart name for a native kind instead of the native spelling itself —
an allomorph satisfies a native constraint in exactly the same cases it satisfies the boxed one
(raku: `sub f(str $x){}; f(<42>)` binds the `IntStr`'s string half). Regression coverage added to
`t/nativecall/light-call-native-scalar-params.t`.

## A second regression caught by CI's battery gate

An Int/Str-valued enum constant hit the same class of bug from a different angle: the `Mixin`/enum
arms of `fast_type_check_tagged`/`fast_type_check` special-cased `(T::Int, EnumValue::Int(_))` and
`(T::Str, EnumValue::Str(_))`, but not the `NativeInt`/`NativeStr` counterparts, so an enum whose
values carry a plain Int/Str — the `enum CBORMajorType (CBOR_UInt => 0, ...)` shape the vendored
`CBOR::Simple` module uses to name its wire-format tags — was wrongly rejected by a native `int`/`str`
parameter. This one did not surface in `make test`; it surfaced in CI's `scripts/battery-testsuite.sh`
gate, which regressed `CBOR::Simple`'s own upstream suite (`01-basic.rakutest`, `04-tags.rakutest`)
below its recorded baseline. Root-caused with a temporary `MUTSU_DEBUG_NATIVE` eprintln at the check
site (`write-medium-uint(CBOR_UInt, $value)` binds the enum constant to a native `int $major-type`
parameter) — removed before landing. Fixed by adding `NativeInt`/`NativeStr` to the same enum arms.
Regression coverage added; the battery gate went from 305/326 to 307/326 passing files (net gain, no
new regressions) after this fix.

mutsu does not yet unbox an accepted enum value to a plain `Int` at the native-int bind site — a
pre-existing gap shared with the general binder (`sub f(int $x is rw)` forces that path and shows the
same non-unboxed result), not a regression this PR introduces and not in scope to fix here.
`CBOR::Simple`'s own code only depends on the value's arithmetic behaving correctly, which the full
upstream suite (74/74, 39/39 on the two previously-regressed files) confirms it does.

## Still open on #8686

Phase 1's first bullet (dispatch-chain `Symbol::intern` re-derivations) is
[#8690](https://github.com/tokuhirom/mutsu/issues/8690). Phase 2 (admitting a simple native `is rw`
scalar parameter to the light paths — the actual `JSON::Fast`/#8673 blocker) needs a `Proposed` ADR
before any implementation attempt. Phase 3 (re-profile once 0–2 land) is unstarted. #8686 stays open.
