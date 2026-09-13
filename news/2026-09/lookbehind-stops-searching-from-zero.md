# A look-behind stops searching from position zero

Round 22 of the YAML-throughput investigation ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)),
and the first round of it that removes a **growth rate** rather than a constant.

Rounds 11-21 cut the 60-row benchmark from 8.130 Bn instructions to 1.199 Bn
(-85%), each by finding work that should not happen and removing it. None of
them changed how the cost scales with the input, and a measurement taken after
round 21 showed why that mattered: against rakudo v2026.07 on the same
container, mutsu was *ahead* on a 10-row document (0.061s vs 0.067s parse) and
4.01x behind on a 240-row one, because mutsu's instruction count grew as
`n^1.225` while rakudo's effective per-row cost *fell* over the same range. A
gap that widens with the input is not a constant-factor problem, so no number of
further rounds of the same kind would have closed it.

This round removes that exponent. On the 60-row document instructions go
2,417,734,204 -> 2,065,297,646 (-14.6%); on the 240-row document
13,207,178,233 -> 7,632,276,629 (**-42.2%**), and the growth from 60 to 240 rows
goes **5.46x -> 3.70x — `n^1.225` -> `n^0.943`, linear**. The parse-only wall
clock against rakudo at 240 rows goes 4.01x -> 2.49x, and what remains of the
ratio's drift with size is rakudo being *sublinear* (MoarVM's JIT warming up),
not mutsu being superlinear.

## The mechanism

`<?after X>` and `<!after X>` ask whether `X` matches ending exactly at the
current position. The engine answers by running `X` *forward* from a candidate
start and checking whether it ends at `pos` — and it tried every start from 0:

```rust
for start in 0..=pos {
    if self.regex_match_end_from_caps_in_pkg(pattern, chars, start, pkg)
        .is_some_and(|(end, _)| end == pos) { … }
}
```

So one look-behind costs O(pos). A negative look-behind costs it every time,
since establishing that nothing matches means exhausting the range.

YAMLish's `token block-ws(Str $indent)` opens with
`<.space>* [ <!after <.alnum>> <.comment> … ]*` and is evaluated once per block
entry. O(entries) evaluations x O(document) per evaluation is the `n^1.2`.

A start earlier than `pos` minus the most `X` can consume cannot end at `pos`,
so those starts are provably unreachable. `regex_lookbehind::lookbehind_start_floor`
computes that bound and the search begins there.

## Two things the bound has to get right

**It counts grapheme clusters, not `char`s.** One `<.alnum>` can span a base
character plus any number of combining marks, and `\r\n` is a single cluster
(`regex_helpers::grapheme_end`). A character bound would therefore be unsound,
so the floor is found by stepping *back* that many clusters with the same rule
`grapheme_end` steps forward by. The atoms whose cluster handling does not fit
that rule — `\n`, `\N`, `\s` — are simply not bounded.

**Every shape it does not model returns `None`**, which means "search from 0" —
the behaviour before this change. So an unbounded quantifier, a subrule call, an
interpolated pattern or a future atom kind costs a missed optimization, never a
wrong answer. `Some` is returned only for a sequence of single-cluster atoms,
zero-width assertions, transparent groups, alternations (the longest branch) and
bounded `** a..b` repetitions.

The search order is unchanged for every start that survives, so a positive
look-behind with captures still resolves to the same match: only starts that
could not match were dropped.

## How it was found

Rounds 11-21 all measured one document size, which is structurally unable to
see an asymptotic term — and so this ticket's "item 3" ("candidate enumeration…
the branching multiplies. Not yet measured directly") survived eleven rounds.
Three measurements located it:

1. **Two sizes, not one.** 60 vs 240 rows gave the exponent, and the excess over
   linear scaling (26.8% of the 240-row run) ranked the functions contributing
   to it — all of them in the core matching loop, growing 8-16x for 4x input.
2. **Is it memoizable?** A temporary counter over
   `(package, rule, arguments, remaining)` said no: subrule evaluations are
   *exactly* linear (87,211 -> 689,491 for 30 -> 240 rows) at a constant 9.24x
   multiplicity. So the superlinear work was inside a single evaluation, and a
   packrat-style memo — the obvious guess — would have been a constant-factor
   change.
3. **Which rule?** A thread-local rule stack attributing every single-atom match
   to its innermost enclosing `<subrule>` named `block-ws` outright: 2,168 atom
   matches per evaluation at 60 rows, 8,648 at 240 (4x the document, 4x the
   per-evaluation work), and 4,151,040 of the parse's 4,150,080
   `matches_named_builtin` calls. After the fix it leaves the top of that table
   entirely.

The lesson worth keeping: **profiling one input size cannot find a growth-rate
bug, however carefully the attribution is done.** A flat profile at one size and
a flat profile at two sizes are different claims, and only the second one rules
out an exponent.

## What is left

`document-prefix` and `empty-document` also scan the whole document per
evaluation, but they are evaluated a fixed number of times (10 and 4), so they
are O(n) in total rather than quadratic — about 240k atom matches at 240 rows,
worth a look but not an exponent. Everything else is as round 21 left it.
