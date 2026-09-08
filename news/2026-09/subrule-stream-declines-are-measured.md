# The `<subrule>` streaming residue is measured, and it is not what the ticket ranked

[#7548](https://github.com/tokuhirom/mutsu/issues/7548) lists six shapes the
streamed `<subrule>` path declines, "roughly in descending order of how often
they come up", and then says plainly what to do before opening any of them:

> Before opening any of these, measure how often the declined shapes actually
> occur: the streamable verdict is memoized per (package, subrule atom text) in
> `regex_call_graph.rs`'s `STREAMABLE`, so a counter there would say directly
> which of the six is worth the work.

That measurement now exists, and the ordering it produces is close to the
reverse of the guess.

## The instrument

`Interpreter::subrule_call_is_streamable` answered a bare `bool`, so a decline
carried no reason. It is now `subrule_call_stream_decline`, answering
`Option<StreamDecline>` — a fieldless enum with one variant per declined shape —
and every `<subrule>` call reaching `drive_named_subrule_candidates` reports
exactly one verdict to a `MUTSU_VM_STATS` histogram. The verdict is memoized
exactly as the boolean was, so the reason costs nothing on a repeated call.

Two of the buckets needed splitting before they could say anything. The
reachability walk's "may re-enter" answer conflated a real call cycle with an
edge it simply could not resolve, and the unresolvable half conflated three
mechanisms with completely different prices. Both are now separate variants.

`scripts/subrule-stream-survey.sh` aggregates the histogram over a file list, so
the table below is reproducible rather than a claim.

## The measurement

Whole roast whitelist plus `t/` — 5 276 files, of which 167 make at least one
subrule call; 8 454 calls:

| verdict | calls | share | #7548's rank |
|---|---:|---:|---|
| **streamed** | 4 632 | 54.79% | — |
| not-a-rule | 1 978 | 23.40% | not listed |
| **callee-interpolates** | **796** | **9.42%** | **item 5** |
| call-arguments | 345 | 4.08% | item 4 |
| several-candidates | 307 | 3.63% | item 3 |
| reenters-own-name | 183 | 2.16% | **item 1** |
| dynamic-rule-param | 123 | 1.45% | item 6 |
| callee-edge-unresolvable | 31 | 0.37% | item 6 |
| custom-how-grammar | 26 | 0.31% | item 6 |
| ignore-mark | 18 | 0.21% | item 6 |
| **proto-candidate** | **14** | **0.17%** | **item 2** |
| callee-is-grammar-method | 1 | 0.01% | item 6 |
| symbolic-indirection, lr-key-active, seed-consulted, reachable-set-too-large | 0 | 0.00% | items 1 / 6 |

## What it says

**Item 5 is the largest clearable residue, and it is also the cheap half.** The
ticket puts "a body that splices a value into its own pattern text" fifth and
files it under "widening the analysis is the cheap half of items 4-6". It is in
fact 796 calls — more than items 1, 2 and 3 put together — and it is one
mechanism, not a family: splitting the old unknowable bucket three ways shows
the grammar-method case, which no widening could ever fix, is **1 call**.

**Item 2 is measured dead.** A proto/`multi` subrule declines 14 calls in the
entire corpus, 0.17%. Item 1, the "genuinely hard residue" the ticket ranks
first, is 183 — sixth place, and less than a quarter of item 5. Building the
seed-loop continuation machinery would buy less than a quarter of what
tightening the interpolation check buys, at many times the cost.

**`not-a-rule` is not a residue** and must not be read as one. It is `<ws>`,
`<alpha>`, `<sym>` and friends: builtin assertions and character classes that
cannot dispatch to a user rule and contain no `{ … }` block to over-fire.
Excluding them, the streamed share is **71.5%** (4 632 of 6 476).

**Three of the six shapes never occurred once**: `<::(EXPR)>` symbolic
indirection, an already-LR-active key, and the mid-stream seed-consulted
fallback.

## The next slice, named precisely

Per file, item 5 is two mechanisms and 90% of it is the first:

- **596 of 796 are `t/yaml-battery.t`** — `YAMLish`'s indentation-driven rules,
  which interpolate a **typed** rule parameter: `token block-ws(Str $indent) {
  … <.comment> <.line-break> $indent <.space>* … }`. A `Str`-declared parameter
  provably cannot be spliced as pattern *source*
  (`interpolate_bound_regex_scalars` splices a `Regex`-valued scalar; anything
  else interpolates as a literal), so such an interpolation can never introduce
  a rule call. The analysis refuses anyway, because
  `pattern_text_is_static_outside_code_blocks` sees only the pattern text and
  not the rule's signature.
- **69 are `roast/integration/advent2013-day18.t`** — a different mechanism:
  `rule deal { :my %*PLAYED = (); <hand>+ % ';' }`. A `:my` is a *declaration*,
  not an interpolation, and splices nothing; the sigil scan does not know that.

Both are static, sound widenings of one function, and together they are ~84% of
the residue. Neither is attempted here: this entry is the measurement the ticket
asked for, and the widening is its own slice — the value of doing them in that
order is exactly that the priority order changed.
