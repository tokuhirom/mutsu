# Enum value fails UInt type check (`INTERNAL_ERROR ~~ UInt` is False), aborting attribute assignment

## Affected tests

- `t/http2-frame-serializer.rakutest`: the file **aborts at line 131** with

  ```
  Type check failed in assignment to $!error-code; expected UInt but got Int
  ```

  when constructing `Cro::HTTP2::Frame::RstStream.new(error-code => INTERNAL_ERROR)` (`Cro::HTTP2::Frame` declares `has UInt $.error-code`; `INTERNAL_ERROR` is a value of `enum ErrorCode`). Every test from line 131 onward never runs, so the file emits no TAP plan and exits rc=1. This is why the file shows "ok=8 notok=3, plan missing" — fixing this unlocks the rest of the file (RstStream, Settings, PushPromise, Ping, GoAway, WindowUpdate, Continuation, and the two `test-multi` split tests).

## Repro

```raku
enum ErrorCode <NO_ERROR PROTOCOL_ERROR INTERNAL_ERROR>;
class RstStream { has UInt $.error-code is required; }
say INTERNAL_ERROR ~~ UInt;                       # mutsu: False   raku: True
my UInt $u = INTERNAL_ERROR;                      # mutsu: dies    raku: ok
say RstStream.new(error-code => INTERNAL_ERROR);  # mutsu: dies    raku: ok
```

mutsu: `Type check failed in assignment to $u; expected UInt but got Int` (note it already reports the value as Int — the enum's underlying type — yet still fails). Plain `Int` values pass (`RstStream.new(error-code => 2)` works).

## Root cause

`src/runtime/types/type_matching.rs:475-486` — the `constraint == "UInt"` early-return arm matches only:

```rust
ValueView::Int(i) => i >= 0,
ValueView::BigInt(n) => ...,
ValueView::Nil => true,
ValueView::Package(name) => name == "UInt" || name == "Int",
_ => false,
```

A `ValueView::Enum { .. }` value falls into `_ => false`. The generic enum-compatibility check at line 496-501 (`if let ValueView::Enum { enum_type, .. } = value.view() ...`) sits AFTER this early return, so it is never reached for UInt. In Raku an Int-based enum value IS an Int, and `UInt` is `subset UInt of Int where * >= 0`, so any non-negative enum value must match.

(Recent PR #6135 "uint-failure-and-subset-nominalization" touched UInt/subset logic but did not cover the Enum representation.)

## Fix direction

Add an Enum arm to the UInt block in `type_matching.rs:475`:

```rust
ValueView::Enum { value, .. } => match value {
    crate::value::EnumValue::Int(i) => *i >= 0,
    _ => false,
},
```

(`EnumValue` is `src/value/mod.rs:1232-1237`: `Int(i64) | Str(String) | Generic(Box<Value>)`; a `Generic` holding an Int could be handled too, but `Int` covers real-world enums.)

While there, check the sibling nominal-subset shortcuts in the same file for the same blind spot (e.g. any other `constraint == "..."` early-return that matches on Int views but not Enum) — at minimum verify `INTERNAL_ERROR ~~ Int` (works today via a different path) and `~~ Numeric`/`~~ Real` against raku.

Risk: low — this widens acceptance to match Rakudo; the only hazard is a multi-dispatch tie changing where a candidate distinguishes `UInt` from an enum type, which roast S12/S02 enum tests will catch on CI.

## Verification

- The 3-line repro above matches raku (`True`, no death, object constructed).
- `t/http2-frame-serializer.rakutest` reaches `done-testing` and prints a plan (subtests 4/5 and 11 additionally need `http2-lexical-sub-lost-after-routine-return.md` and `http2-rw-param-buf-element-assign.md`; with all three fixed the file should be 20/20-ish — establish the exact count under raku first: `cd tmp/cro-work/C_RO_CRO_HTTP_...; raku $(cat ../inc-paths.txt) -I lib -I t t/http2-frame-serializer.rakutest`).
- New pin `t/enum-uint-subset.t` with the repro.
- `make test` + CI roast (S12 enums, S02 subset tests).
