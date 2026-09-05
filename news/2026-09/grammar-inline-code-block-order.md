# A `make`-bearing embedded code block now runs in match order

A grammar `rule` whose embedded `{ … }` blocks used `make` executed them in the
wrong order relative to its subrule calls and to its own later blocks, and a
later block could not see what an earlier block had `make`d. The
`Language/grammar_tutorial.rakudoc` line-679 example printed
`param a` / `end ` / `func f` on mutsu where raku prints `func f` / `param a` /
`end f`.

## Root cause

`code_block_defers_to_reduce` (`src/runtime/regex/regex_helpers.rs`) classified a
block by scanning its source text: a block mentioning the bare identifier `make`,
or any `$*`/`@*`/`%*` dynamic variable, was **deferred**; everything else ran
inline where the cursor reached it (ADR-0009 part B, `eval_regex_inline_code`).

A deferred block was stored as a `CodeBlockContext` on the capture node and
replayed by `reduce_run_code_blocks` during the post-order reduce walk over the
finished capture tree. That walk visits children before the parent — so
`<parameter>`'s block ran before the parent rule's *first* block — and a node's
`ast` (`.made`) was committed only as its reduce step *ended*, so a sibling block
replayed in the same step read `Nil`. When one block of a rule mentioned `make`
and another did not, the two ended up in *reverse* source order, because one
deferred and the other did not.

The deferral existed for a real reason: `make` sets `$/.made` on the node being
built, and during matching that node's `Match` does not exist yet.

## What was measured

Re-measuring the ticket's repro and its three variants on current `main`
confirmed all of them, and measuring `raku` settled three design questions the
ticket had left open or answered wrongly:

- **`make` is not undone on backtracking in raku, and is not last-write-wins in
  any node-scoped sense.** `'abc' ~~ / 'a' [ 'b' { make 1 } 'q' ]? 'b' 'c' /`
  makes `1` in raku even though the only execution of the block was on a path the
  engine abandoned, and it overwrites an earlier `make 9` that *was* on the
  winning path. The ticket's constraint 2 asserted the write "must be undone on
  backtracking"; raku does not undo it.
- **A failed subrule's `make` does not reach its parent** — the subrule has its
  own cursor.
- **Blocks re-run on backtracking in both implementations**, so the winning
  path's `make` is the last one executed on the successful path.

## Fix

Embedded `{ … }` blocks that use `make` now run inline, in match order, like
every other side-effect block. The produced value travels on the capture delta:

- `eval_regex_inline_code` clears the `make` slot (`env["made"]`) around the body
  and returns what the body left there as part of a new `InlineCodeOutcome`,
  instead of letting it leak into the shared env.
- The code atom puts that value on `RegexCaptures::ast` of the delta it returns.
  `CapStore::merge_delta` folds it into the capture store under a new
  `Undo::Ast` trail entry, so an abandoned branch's `make` is rewound with the
  rest of its captures, and `merge_regex_captures` / the group- and
  alternation-level bubbling sites carry it up through `[ … ]`, `|` and `||` via
  a shared `adopt_inline_ast` helper — it follows exactly the same routing the
  block list already had, so a *subrule*'s value stays on the subrule's own
  `CapNode` (where `$<child>.made` reads it) instead of bubbling into the parent.
- `eval_regex_inline_code` hangs the accumulated value off the `$/` it builds, so
  a later block of the same rule reads it back as `$/.made`.
- The reduce walk is unchanged for `:actions` dispatch, `silent_caps` and the
  `.made` an action produces. It now *seeds* `reduce_run_code_blocks` with the
  node's inline value rather than clearing it, so a rule that mixes an inline
  block with a deferred one does not lose the inline half, and it republishes the
  node's value into `env["made"]` on the no-deferred-blocks path, which is where
  `Grammar.parse` and smartmatch read the whole match's `.made` from.

Because the inline path already has the rule's live dynamic scope, only the
`$*`-dynamic-variable half of the old text scan survives:
`code_block_defers_to_reduce` is now exactly `code_block_uses_dynamic_var`. A
block whose *string literal* merely contained the word `make` no longer defers.

## One follow-on: ordered alternation stopped skipping value-producing blocks

mutsu evaluates every branch of an ordered alternation (`||`) eagerly, because a
later branch's candidate ends are what let an enclosing pattern backtrack into
it. Since a branch after one that already matched is one raku's cursor may never
reach, `SPECULATIVE_ALT_BRANCH` skips its `{ … }` blocks so their side effects do
not fire (`t/ordered-alternation-loser-code-block.t`; the shape that motivated it
is `Config::TOML`'s `\\ [ <escape> || . { die "bad escape sequence" } ]`).

While `make` deferred, that skip was harmless: the block was recorded on the
node either way and the reduce walk ran it if the branch turned out to be the
one that matched. Once the block runs inline, skipping it also throws away the
*value*, and an atom after the alternation reads that value back while the match
is still running — which is exactly YAMLish's plain-scalar resolver
(`Schema::JSON`'s `regex TOP { [ <element> <.ws> || <plain> ] { make
$/.values[0].ast } }`). A YAML block scalar whose first line began with a digit
came back as `Any`, because `<element>` matched the leading integer, the overall
parse then needed the `<plain>` branch, and `<plain>`'s `{ make ~$/ }` had been
skipped. The battery gate caught it (`YAMLish test-harness.rakutest`).

So the skip is now narrowed by `code_block_produces_value`: a pure side-effect
block in a speculative branch is still skipped (it always succeeds, so the
branch's candidate ends are unchanged), while a `make`-bearing one runs. The
`make` text scan therefore survives, but for this one decision only, and a false
positive merely means such a block runs — which is what mutsu did before
`SPECULATIVE_ALT_BRANCH` existed at all. Pinned by the last three assertions of
`t/grammar-inline-code-block-order.t`.

## Verified

The headline repro and all three of the ticket's isolating variants now match
raku exactly, as do `$/.made` sibling visibility in a plain regex, `$<child>.made`
in a parent block that follows the subrule, and the backtracked-re-run case
(`'aaab' ~~ / (\w)+ { … make … } b /` now runs its block twice and keeps the
winning path's value, as raku does).

Pins: `t/grammar-inline-code-block-order.t` (14 assertions, every one of them
checked against `raku` first), plus the existing
`t/grammar-inline-make-subrule-made.t`, `t/grammar-per-match-dynvar-action.t`
and `t/grammar-reduce-time-dynvar.t`.

## Known residue (measured, not regressions)

- A `make` executed only on a path the engine later abandons is **rewound** by
  the match trail; raku keeps it. mutsu behaved this way before the change too;
  the trail is the sound mechanism and matching raku here would mean deliberately
  leaking an abandoned branch's write.
- `make` inside a *positional capture group* (`/ a ( b { make 5 } ) /`) attaches
  to the enclosing regex on one of mutsu's two capture-group paths, where raku
  attaches it to the group (`$0.made`). Unchanged by this work — the value
  follows the block routing that was already there.
- A block that mentions a `$*` dynamic variable still defers, so it can still run
  out of order relative to an inline sibling. Moving that half inline needs the
  per-match `:my $*x` value to be recorded at match end instead of re-derived at
  reduce time; recorded as `todo/deep/grammar-dynvar-code-block-still-defers.md`.
- A *side-effect-only* block in a later `||` branch still does not run when that
  branch turns out to be the one the overall match needed — the pre-existing
  eager-alternation approximation described above. The real fix is to evaluate a
  later branch lazily (or re-evaluate it for real once the engine commits to it)
  rather than to classify its blocks by text; recorded as
  `todo/deep/ordered-alternation-branches-evaluated-eagerly.md`.
