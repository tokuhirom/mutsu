# ADR-0093: A core operator rakudo does not declare is *shadowed* by a user declaration, not extended

- Status: Accepted (implemented)
- Date: 2026-09-12
- Supersedes: nothing
- Extends: [ADR-0071](0071-native-operators-are-dispatch-candidates.md) (a natively
  implemented operator is a dispatch candidate, not a fallback)
- Related: [ADR-0044](0044-listops-are-routines-not-a-syntactic-rewrite.md) (a core
  listop is a routine, and a user `multi` of its name *extends* its candidate set)
- Closes: [#8006](https://github.com/tokuhirom/mutsu/issues/8006)

## Context

ADR-0071 established that a natively implemented operator participates in its own
multi dispatch as a modelled candidate set. `try_user_infix` ranks a user
`multi infix:<op>` against that set; when the core set wins it reports "no user
candidate" and the native implementation runs. That is right, and measured
against rakudo, for the arithmetic and comparison families:
`multi infix:<+>(Q, Q)` leaves `1 + 2` printing `3`.

But ADR-0071 answered only half of a question it did not know it was asking. It
modelled *how narrow* the core candidates are; it never asked **whether the
operator has core candidates at all**. Two places read the un-modelled case, and
they read it differently:

- `core_infix_candidate_wins` treated `core_infix_shape(name) == None` as "no
  core candidate to rank", so the user candidate won — right.
- `call_infix_fallback` (`src/vm/vm_flipflop_ops.rs`) went on running a builtin
  anyway when *no* user candidate matched — and its last resort was
  `call_function_compiled_first(name, ...)`, a call to a core routine of the
  operator's **bare** name.

That last step is the bug, and it is not one operator's. For `infix:<cross>`,
rakudo has **no `&infix:<cross>` routine at all**:

```
$ raku -e 'say (1,2) cross (3,4)'
===SORRY!=== Two terms in a row
```

`cross` is a core list-operator `sub cross(...)`, and mutsu spells it as an
infix as a convenience, reaching that very `sub` through the bare-name fallback.
So in rakudo, declaring `multi infix:<cross>` does not join anything — it
installs a *fresh lexical routine*, and a call its candidates do not accept is
`X::Multi::NoMatch`:

```raku
class Q {}
multi infix:<cross>(Q $a, Q $b) { "QC" }
say (1,2) cross (3,4);
```

```
raku : Cannot resolve caller infix:<cross>(List:D, List:D); none of these signatures matches:
           (Q $a, Q $b)
mutsu: ((1 3) (1 4) (2 3) (2 4))      # the core list operator answered anyway
```

It bit `Math::Vector` 0.6.0, whose `t/01-basics.rakutest` declares a
dimension-guarded `multi infix:<cross>` and then asserts that mismatched-dimension
calls die. Under mutsu those `dies-ok` calls fell through to the builtin and got
a `Seq` back, so three of its 201 assertions failed.

Note the contrast with ADR-0044, which is not a contradiction but the same rule
seen from the other side: a user `multi splice` *does* extend the core, because
rakudo really does declare `sub splice` as a multi. The question was never
"operator vs. listop" — it is, and always was, "does rakudo declare a routine of
**this** name".

## Decision

**"Does this operator have a core candidate set" is one question with one
answer, and the answer is a property of the operator's name measured against
rakudo.**

`src/runtime/native_infix_dispatch.rs` gains `CoreInfixCandidates`, which both
the ranking and the fallback now consult:

1. **`Modelled(shape)`** — rakudo declares `&infix:<op>` and mutsu models the
   type constraints of its two-positional candidates (ADR-0071's table). A user
   candidate joins the set and must out-narrow the core one to take the call.
2. **`Unmodelled`** — rakudo declares `&infix:<op>`, but mutsu has no type table
   for it (`minmax`, `min`, `max`, `eqv`, `x`, `Z`, the set operators, ...). A
   matching user candidate takes the call; when none matches, the native
   implementation still answers, exactly as rakudo's own core candidates do.
   This is the pre-ADR-0093 behaviour, preserved unchanged.
3. **`Shadowing`** — rakudo has **no** `&infix:<op>` routine. mutsu's infix
   spelling of the name is a convenience over a core *list-op sub* of the same
   bare name (`cross`, `zip`, `roundrobin`, and mutsu's own extras such as
   `sum`, `flat`, `unique`, `squish`), or the operator is purely user-defined
   (`infix:<@@>`). A user declaration replaces it outright: once the user's own
   candidates have declined, `call_infix_fallback` raises `X::Multi::NoMatch`
   naming only their signatures, instead of reaching the bare-name core routine.

**The classification is vendored from rakudo, not hand-reasoned.**
`src/runtime/core_infix_names.rs` holds all 143 `&infix:<...>` keys of rakudo's
`CORE::` package, measured on rakudo 2026.07 with

```raku
CORE::.keys.grep(*.starts-with("&infix:"))
```

sorted in byte order and binary-searched. A name absent from that table is
`Shadowing`. Putting the whole set in one measured table is what makes the third
answer safe: guessing which spellings mutsu adds on its own would have been a
standing source of wrong answers in both directions, whereas the complement of a
vendored list is exact by construction.

**The guard is placed after every operator-named path has declined.**
`try_user_infix`, the chain-op loop, the `call_user_routine_direct(infix:<op>)`
call and the reduction in `apply_reduction_op` all run first and are untouched;
only the *bare-name* fallbacks below them are shadowed. That ordering is what
keeps `minmax`, `min`, `max`, `x` and the rest correct even though mutsu does
not model their candidate types — they are answered by name before the guard is
reached, and are classified `Unmodelled` besides.

**And it fires only for a `multi`.** The gate is `has_multi_function_cached`,
the same one ADR-0071 rule 5 already uses for "a plain `sub infix:<op>` is a
lexical shadow, not a candidate": such a `sub` either resolved above and
replaced the operator outright, or it is not visible here at all, so only a
`multi` can reach the guard having declined the call. That is also what keeps a
routine merely *declared* in a module and never exported from being reported as
a candidate set the caller cannot see — roast
`S06-operator-overloading/imported-subs.t` has a fixture that declares
`sub infix:<notthere>` without exporting it and requires `3 notthere 4` to stay
`X::Syntax::Confused` ("Two terms in a row"). `user_infix_override` alone is a
cheap "some in-scope unit declared this name" probe and is too loose to carry
the decision on its own.

## Alternatives considered

### A. Make the shadowing rule blanket: any unmodelled operator is shadowed

The smallest possible change: treat `core_infix_shape(name) == None` as
`Shadowing` everywhere and delete the third case.

**Rejected on measurement.** Only 22 operators are modelled; rakudo declares
143. `minmax`, `min`, `max`, `eqv`, `x`, `cmp`, `Z`, `..`, `but`, `does`, `o`
and the whole set-operator family would have started raising `X::Multi::NoMatch`
for calls rakudo answers with a core candidate — for instance
`multi infix:<minmax>(Q, Q) { }; say (1,2) minmax (3,4)`, which is `1..4` in
rakudo. The blanket rule trades one wrong answer for a hundred.

### B. Derive the classification from which fallback path answers

Do not classify by name at all: shadow exactly the bare-name call in
`call_infix_fallback`, on the reasoning that a core *operator* is always reached
through the operator's own name and only a core *sub* is reached by the bare
one.

**Tempting, and very nearly right** — it needs no table and it happens to give
the correct answer for every operator measured. It was rejected because it makes
a semantic classification depend on the incidental order of a fallback chain:
the day an operator rakudo does declare acquires a bare-name implementation (or
`apply_reduction_op` loses a row), the operator silently changes meaning with
nothing to review. The vendored table states the fact being relied on, and the
`#[cfg(test)]` rows in `core_infix_names.rs` pin it. The placement rule above
keeps B's benefit — the guard still sits after every operator-named path — while
the *decision* is the table's.

### C. Detect the shadowing at declaration time, in the parser

Notice `multi infix:<cross>` while parsing and stop compiling `A cross B` to an
`InfixFunc` with a builtin behind it.

**Rejected.** The declaration can arrive from an import or an `EVAL`, so the
parser is not where the answer is known; and the parse-time route would have to
reproduce the run-time candidate ranking to decide *which* call falls through.
ADR-0071 already settled that this decision belongs at the call site with the
arguments in hand.

## Measured acceptance criteria

Pinned by `t/routines/dispatch/user-infix-op-core-candidate-set.t`, which passes
**identically under `raku` and under `mutsu`** (22/22 both ways). Each row runs
in its own `EVAL`, with its own class name, so candidate sets do not leak.

Core-only names — a user declaration shadows them:

| call, with `multi infix:<op>(Q, Q)` in scope | rakudo | before | after |
|---|---|---|---|
| `(1,2) cross (3,4)` | `X::Multi::NoMatch` | the cross product | `X::Multi::NoMatch` |
| `(1,2) zip (3,4)` | `X::Multi::NoMatch` | the zip | `X::Multi::NoMatch` |
| `(1,2) roundrobin (3,4)` | `X::Multi::NoMatch` | the roundrobin | `X::Multi::NoMatch` |
| `Q.new cross Q.new` | the user candidate | agreed | unchanged |
| `(1,2) X (3,4)` | the core `X` — a separate name | agreed | unchanged |
| a `where`-guarded candidate that declines | `X::Multi::NoMatch` | the builtin | `X::Multi::NoMatch` |

The message matches rakudo's line for line, including the call profile and the
candidate list:

```
Cannot resolve caller infix:<cross>(List:D, List:D); none of these signatures matches:
    (Q $a, Q $b)
```

Operators rakudo does declare — the core candidate survives, unchanged:

| call, with `multi infix:<op>(Q, Q)` in scope | rakudo | after |
|---|---|---|
| `1 + 2` | `3` | `3` |
| `(1,2) minmax (3,4)` | `1..4` | `1..4` |
| `1 min 2` / `1 max 2` | `1` / `2` | agreed |
| `(1,2) eqv (1,2)` | `True` | `True` |
| `"a" x 3` | `aaa` | `aaa` |
| `1 cmp 2` | `Less` | `Less` |
| `"a" ~ "b"` | `ab` | `ab` |
| `1 == 1` | `True` | `True` |

A plain `sub` (not `multi`) has always replaced the operator outright, for a
real core operator as much as for a core-only name; both rows are pinned and
unchanged. So is the module-private declaration: `3 notthere 4` with an
unexported `sub infix:<notthere>` somewhere in the loaded code stays
`X::Syntax::Confused`, pinned both by the new test and by roast
`S06-operator-overloading/imported-subs.t`.

`Math::Vector` 0.6.0's `t/01-basics.rakutest` goes from 197/201 to **201/201**,
matching its rakudo baseline. Its three failures (192, 195, 196) were exactly
the `dies-ok` assertions on a dimension-guarded `multi infix:<cross>`.

## Consequences

- `core_infix_shape` is now reached only through `core_infix_candidates`, so
  there is one function answering "does this operator have a core candidate
  set". `core_infix_candidate_wins`'s behaviour is unchanged in both directions:
  neither an `Unmodelled` operator nor a `Shadowing` one has a core candidate to
  rank, so the user's takes the call. What the two now differ on is what happens
  when *no* user candidate matches, which is the fallback's question alone.
- `src/runtime/core_infix_names.rs` is vendored data with a measurement recipe in
  its module doc. Re-measure it when the pinned rakudo moves; a `#[cfg(test)]`
  row enforces the byte ordering its binary search depends on.
- The four hand-copied `X::Multi::NoMatch` construction blocks (two in
  `builtins_operators_fallback.rs`, one in `calls.rs`, one in
  `dispatch_proto_call.rs`) collapsed into one
  `Interpreter::multi_no_match_error`, which the new fallback site also uses —
  so the operator's error is the same shape as every other routine's, for free.
- mutsu's own convenience spellings (`(1,2) cross (3,4)` with nothing declared,
  which rakudo rejects at compile time) are untouched: the guard only fires when
  a user `infix:<op>` of that name is actually in scope.

## Known remaining divergence

mutsu still accepts `A cross B`, `A zip B`, `A sum B` and any other bareword as
an infix where rakudo says "Two terms in a row". Making those a parse error is a
separate, much wider change to the speculative word-infix layer
(`parse_custom_infix_word`), and would need its own measurement of what real
code relies on the leniency. This ADR only settles what happens once the user
has declared such an operator.
