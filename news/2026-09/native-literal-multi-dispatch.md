# A literal, and a same-family native type, reach the native multi candidate

```raku
multi sub d(int $x) { "native" }
multi sub d(Int $x) { "boxed" }
say d(5);        # raku: native    mutsu: boxed
```

Two independent defects, found together because the ticket's repro needed both
fixed to answer as rakudo does.

## 1. A literal carried no provenance

The variable spellings were already right (`my int $n = 5; d($n)` answered
`native`), because `unwrap_varref_for_dispatch` reads the source variable's
declared type off the argument's `VarRef` wrapper and
`type_hierarchy_distance_with_var_type` ranks the native candidate ahead of the
boxed one with it. A literal has no source variable, so `var_type` was `None`
and the generic hierarchy distance put `Int` at 0.

**The discriminator is provenance, not the value.** Measured against rakudo
v2026.07: `d("7".Int)` answers `boxed` even though it produces an in-range boxed
`Int` at runtime, and `d(2**70)` answers `boxed` because it does not fit the
native width. A rule based on the value alone gets the first of those wrong,
which is why the fix has to carry a compile-time fact to the dispatcher.

`OpCode::CallFunc` / `CallFuncNamed` therefore gained a `literal_native_args:
u32` — a bitmask of the argument positions written as a literal of a type with
a native counterpart (`Int`, `Num`, `Str`, and a negated numeric literal, which
parses as `Unary { Minus, Literal }`). `exec_call_func_op` publishes it on the
interpreter for the duration of the call, and `unwrap_varref_for_dispatch_at`
hands a marked position the native type its value's own kind implies.

A plain `u32` rather than the fifth `arg_sources` entry shape the ticket
suggested: that constant is elided whenever every position is `NIL`, which is
nearly every call site, so marking literals there would materialize it — and
pay its per-call decode — crate-wide, to serve a ranking step that only runs
when a `multi` declares both a native and a boxed candidate. The mask costs one
`u32` store per call and nothing at all when zero.

The multi-resolve cache had to learn the same fact. Its key already carried the
declared type of a `VarRef` argument behind a reserved marker; a literal's
synthetic native type now goes in beside it under `LITERAL_NATIVE_KEY`. Without
that, `d(5)` and `d("7".Int)` — the same `Int` value, different provenance,
different rakudo answer — collided on one key and whichever ran first decided
for both.

## 2. Native types were compared by spelling, not by family

Even *with* a source variable, only an identically-spelled native constraint
beat `Int`, so `my int $n` lost an `int64`, `int32` or `int8` candidate to the
boxed one. rakudo ranks every native type in the same **family** equally — two
same-family candidates are an "Ambiguous call" there rather than one winning —
and the family boundaries are exactly:

| family | members | boundary evidence |
| --- | --- | --- |
| signed | `int`, `int8`…`int64` (+ the C aliases) | `my int $n` picks `int8` over `Int` |
| unsigned | `uint`, `uint8`…`uint64`, `byte` (+ aliases) | `my int $n` against `uint`/`Int` answers `Int`; a literal against `byte`/`Int` answers `Int` |
| num | `num`, `num32`, `num64` | `my int $n` against `num`/`Int` answers `Int` |
| str | `str` | |

`native_family` replaces the string equality, and an integer literal's family is
the **signed** one, which is what makes `d(5)` reach `int64` but not `uint`.

## The fallback distance had to become relative

Ranking every non-family constraint at a flat penalty of 1 collapsed the
distinctions *between* them: `Int` and `Numeric` both came out at 1 for an
integer argument, so `multi rt71754(Int)` / `multi rt71754(Numeric)` tied and
picked the wrong one (`roast/S06-advanced/callsame.t`, caught by the roast
slice, not by `make test`). The fallback is `1 + type_hierarchy_distance(...)`
now, which keeps a same-family native (0) ahead of everything while preserving
the ordinary hierarchy order among the rest.

## Pins

`t/native-literal-multi-dispatch.t` — new, 23 assertions, **each also passing
under rakudo v2026.07**: the literal repro across all three native families,
declaration order, the two provenance rows that must stay `boxed`
(`"7".Int`, `2**70`), both variable spellings, the family rule at four widths
for both a variable and a literal, all four boundary rows, and a native
candidate still beating `Any`.

`make test` (3788 files, 39838 tests) and the 263 whitelisted `roast/S06-*`,
`S09-typed-arrays/*`, `S12-*` and `S02-types/*` files (16780 assertions) are
green.

## Residual

`d(5 + 0)` still answers `boxed` where rakudo answers `native`: rakudo
constant-folds the expression back to a literal, and mutsu does not, so the
call site's mask leaves that position clear. That is a constant-folding gap
rather than a dispatch one — and folding `+` unconditionally is unsound while a
user can overload `infix:<+>` — so it is filed as
`todo/tickets/constant-folding-does-not-reach-multi-dispatch.md`.

Two divergences bounding the rule were also measured and left alone, both about
*acceptance* rather than ranking: mutsu accepts `q(5)` for a lone `multi
q(uint $x)` where rakudo rejects it at compile time ("Calling q(Int) will never
work"), and mutsu picks a winner where rakudo reports "Ambiguous call" between
two same-family native candidates.
