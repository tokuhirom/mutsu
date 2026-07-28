# Regex `{ … }` side-effect blocks run inline, and in-regex `:my` lexicals reach the atoms after them

A plain `{ code }` block inside a regex used to be **deferred**: it was recorded
in `RegexCaptures::code_blocks` during the match and executed afterwards, on the
reduce-time walk that also commits `make`. Raku instead runs it immediately,
left-to-right, while matching — which matters because the whole point of such a
block is usually to compute something the *following* atoms depend on:

```raku
grammar L { token TOP { :my $x = 'n'; { $x = 'y' } <?{ $x eq 'y' }> 'z' } }
L.parse('z')      # raku: matches.  mutsu, before: no match.
```

## What changed

A plain block that needs nothing from the reduce-time walk is a pure side-effect
block and now runs inline on the real interpreter, by the same route ADR-0009
part B established for `<?{ … }>` code assertions. It is not also recorded for
the reduce-time replay, so it still runs exactly once.

Two constructs keep the block on the reduce path, because both depend on an
ordering that only exists after the match:

- **`make`** — a node's AST is built from its *already reduced* children, which
  is what makes `make $<child>.made` work;
- **a dynamic variable** (`$*x`) — a rule's `:my $*x` is one binding per match,
  installed and read back around each node's reduce step, so the node's action
  method sees its own match's value rather than a sibling's.

`code_block_defers_to_reduce` (`regex_helpers.rs`) draws the line, conservatively:
anything that might be either keeps the established behaviour.

Writes the block makes to in-regex `:my` / `:let` lexicals are harvested out of
the env and threaded back through `RegexCaptures::regex_vars`, which the match
trail already knows how to undo on backtracking, so a write on a path that is
later abandoned does not survive it. Writes to an *outer* lexical still reach the
caller's compiled local slots — `'123' ~~ / (\d) { $seen = $/.Str } \d+ /` leaves
`$seen` set — via the same env-diff bookkeeping the reduce-time replay used;
assertions keep ADR-0009's cheaper "leave it in `env`" behaviour so the hot
`<?{ … }>` path does not take on an env snapshot per cursor position.

Three gaps around the same lexicals came out of the work and are fixed with it:

- **Code atoms could not read a `:my` lexical at all.** Neither an assertion nor
  a block ever had `caps.regex_vars` installed in the env it evaluates in, so
  `:my $x = 'y'; <?{ $x eq 'y' }>` was reading an outer `$x` — or nothing.
- **Inline sub-patterns lost them.** A lookaround, a group and an alternative are
  each matched with a *fresh* capture store, so they neither inherited the
  enclosing regex's lexicals nor propagated writes back out. A take-scoped
  `INLINE_REGEX_VARS_SEED` now seeds exactly those, and their arms merge
  `regex_vars` outward. A *subrule* is a different regex and deliberately does
  not inherit them.
- **Subrule arguments were frozen too early.** `instantiate_named_regex_arg_calls`
  renders argument expressions into the pattern text before the match runs. For
  an argument naming a `:my` lexical that is a value that does not exist yet, so
  it baked a permanent `Nil` into the pattern. Such an argument is now left
  verbatim and re-evaluated at match time, where `make_regex_eval_env` supplies
  the lexicals.

## Standalone `:` backtrack control

Separately, a solitary `:` — "commit to the atom just matched, never backtrack
into it" — was rejected outright as `Unrecognized regex metacharacter :`, which
killed the entire rule containing it. It now sets the per-token `ratchet` flag on
the token just emitted, which is precisely what the construct means. `::` and
`:::` are different controls and are untouched, and a `:` with no preceding atom
still errors, as raku does.

## Why

This is the regex half of the YAMLish battery blocker
(`todo/deep/yamlish-block-collections-regex-vars.md`). YAMLish measures the
indentation of every block collection with

```
token root-block {
    :my $new-indent;
    <?before $<sp>=[' ' ** { 0..* } ] { $new-indent = ~$<sp> }>
    $new-indent
    [ <value=sequence($new-indent)> | <value=map($new-indent)> ]
}
```

which needs every one of the above at once: the block runs inside a lookahead,
writes a `:my` lexical, and that lexical is then both interpolated as an atom and
passed as a subrule argument. `YAMLish::Grammar.parse("- 1\n- 2")` now produces
the byte-identical AST raku produces. `load-yaml` still has three non-regex gaps
left, recorded in the ticket.

Pins: `t/regex-inline-code-block.t`, `t/regex-standalone-backtrack-control.t` —
both also pass under raku.
