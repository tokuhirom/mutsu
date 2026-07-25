# Grammar actions now run when the parse fails

Rakudo dispatches a grammar action method the moment its rule reduces — the
instant the rule matches — and never un-dispatches it when the surrounding
pattern later backtracks past it. So a `Grammar.parse` that fails *overall*
still leaves behind the effects of every subrule that did match.

mutsu instead walked the finished match tree in a post-pass, which on a failed
parse is either absent or covers only a prefix. The result: a failed `.parse`
ran **no actions at all**.

`HTTP::Header.parse` depends on exactly the behaviour mutsu lacked. Its start
rule is

```raku
token TOP { [ <message-header> \r?\n ]* }
```

so a header string without a trailing newline makes the `*` commit zero
iterations: `TOP` matches `""`, `.parse` fails — and yet raku has already run
the `message-header` action, which is what populates the header object.
`$h.parse('ETag: W/"1201-51b0ce7ad3900"')` worked in raku and silently did
nothing in mutsu.

## What changed

Three pieces, none of which moves action dispatch into the regex engine:

- **The matcher logs each reduce.** `REDUCED_SUBRULES`
  (`src/runtime/regex/regex_helpers.rs`) records every named subrule that
  matched during an action-driven parse, keyed by `(rule, from, to)` so the
  candidate enumeration cannot log the same reduce twice. It is inert unless
  `:actions` is in play, and never records while the matcher is only measuring
  a declarative prefix or probing the failure position (ADR-0009).
- **A failed `.parse` keeps its partial match.**
  `regex_match_with_captures_full_from_start_tracking_partial` hands back the
  longest match even when it does not cover the whole text, so the failure path
  can dispatch that tree — start rule's own action included, as raku does.
- **The failure path dispatches, then replays.** Whatever reduced *outside* the
  surviving tree (the trailing attempt that made the parse stop short) is
  replayed from the log, keeping only maximal spans so a nested rule is reached
  through its parent's walk and still gets bottom-up order and `.made`
  propagation.

One related hole closed with it: when a lone (non-proto) start rule failed the
declarative-prefix measurement, mutsu skipped the real match entirely as an
optimisation. During an action-driven parse that match has observable side
effects, so the candidate is now handed back and the real match runs and fails.

## Result

The upstream HTTP::UserAgent test suite goes from 23/27 to 25/27 files passing —
`t/010-headers.rakutest` (3 subtests) and `t/050-response.rakutest` (1 subtest)
are now clean. Pinned by `t/grammar-actions-on-failed-parse.t`.

The other half of PLAN.md §8.20 — a per-match `:my $*FINAL` leaking to earlier
segments' actions on a *successful* parse — remains, and the entry now records a
sharper diagnosis: it is an ordering problem between mutsu's two bottom-up
post-passes (code blocks first, then actions), not something that needs
reduce-time dispatch inside the matcher.
