# Dists that load but fail their own test suite (sweep `--run-tests`, 2026-07-25)

Found by running `scripts/dist-compat-sweep.py --run-tests` (n=60, seed 20260719)
on the same sample the load-only sweep uses. That axis runs each loading dist's
own suite with **raku as the baseline** — only files raku itself passes cleanly
are graded — and it is a far sharper frontier than "does it load":

```
=== test-suite axis: 28 load_ok dists ran tests ===
  test_pass          7  27% of graded
  test_fail          5  19% of graded
  test_die          14  54% of graded
```

Raw data: `tmp/sweep-tests.tsv` / `tmp/sweep-tests.log` (gitignored; regenerate
with the command above).

One of the 14 `test_die` rows is already fixed: `Prime::Factor` needed multi
dispatch to honour a named parameter's aliases
(`news/2026-07/multi-dispatch-honours-named-param-aliases.md`), and now matches
raku exactly across all four of its files. The rest are separate root causes.
Three are triaged below; the remainder are un-triaged.

## Triaged

### `String::Splice` — an imported `multi` whose `proto` is not exported is unreachable by name

```raku
use String::Splice;
say splice('', 0, 'Raku');   # raku: Raku      mutsu: Unknown function: splice
say &splice.defined;         # both: True
```

The dist declares `proto sub splice (Str(Any) $, |c) is pure {*};` **without**
`is export`, and its four candidates as `multi splice (...) is export`. So the
name resolves (`&splice` is defined) but the call finds no callable. Note the
method form is a red herring: `''.splice(0, 'Raku')` fails in **raku** too
("Routine does not have any candidates. Is only the proto defined?"), so this was
equally broken before the listop-shadow gate landed
(`news/2026-07/listop-rewrite-respects-user-routine-shadow.md`) — that gate only
changed which error appears. Suspect the call path looks for an exported proto
rather than assembling the exported multi candidates.

### `String::Splice` — spurious octal worry for a bare word inside `<...>`

```raku
my @ans = <0 10 021 1320 02431>;
# mutsu: Potential difficulties: Leading 0 does not indicate octal in Raku; use 0o21 ... (found 021)
# raku:  (no warning)
```

Inside a word-quote list these are **strings**, not numeric literals, so the
leading-zero worry must not fire. Independent of the item above; cosmetic but
noisy (four warnings on one line).

### `Text::Sorensen` — `.value` on Any

Reaches subtest 4 of 21, then dies with
`No such method 'value' for invocant of type 'Any'` at `t/01-basic.t:15`. Whatever
the code expects to be a `Pair` there is `Any` in mutsu. Needs reduction.

### ~~`Locale::Dates` — `Unknown function: Dates`~~ — FIXED

Was `Locale::Dates($locale)`, i.e. invoking a user class as a coercion. mutsu had
that path for built-in types, roles and enums but not for user classes. Fixed in
`news/2026-07/class-type-object-coercion-call.md`; both of its files now match
raku (24 subtests).

**Left over from that fix:** the same call form on a **subset** is still
unsupported — `subset Sm of Int where * < 10; Sm(5)` returns 5 in raku,
`Unknown function: Sm` in mutsu. Different mechanism (coerce to the base type,
then check the constraint).

## Un-triaged `test_die` / `test_fail`

`RSV`, `P5seek`, `Date::YearDay`, `PSpec`, `Array::Rounded` (fail);
`Math::Interval`, `Native::Overflow`, `App::SudokuHelper`, `P5tie`,
`Mathematica::Serializer::Encoder`, `Hash::Restricted`, `Crypt::RC4`,
`Random::Choice` (die).

## How to work this list

Per `docs`/PLAN §B4 and the standing rule: **take the raku baseline first**
(`raku -I <dist>/lib <test>`), reduce to a minimal repro, verify the repro is
valid Raku by running it under raku, fix generally, pin in `t/`, one PR per root
cause. Do not trust the bucket label — in the load-only axis 4 of 6 "real mutsu
failures" turned out to be missing dependencies or harness artefacts.
