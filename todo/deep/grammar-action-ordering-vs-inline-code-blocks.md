# A `make`-bearing embedded code block runs at reduce time, not in match order

A grammar `rule` whose embedded `{ … }` blocks use `make` executes them in the
wrong order relative to its subrule calls and to its own later blocks, and a
later block cannot see what an earlier block `make`d.

This is the deep, design-level sibling of the four embedded-block tickets
resolved on 2026-08-26 (`news/2026-08/dollar-numbered-capture-*`,
`dollar-cent-cursor-*`, `dollar-plusplus-state-*`,
`grammar-embedded-custom-assertion-*`). Those were all about *what a block can
see*; this one is about *when a block runs*, and it cannot be fixed without
changing where `make` writes.

## Repro (use this source text verbatim)

Transcribing the `'sub'` / `'('` literals as `"sub"` / `"("` changes what the
matcher does and reproduces something else — keep the single quotes.

```raku
grammar G {
  rule TOP { <function-define> }
  rule function-define {
    'sub' <identifier>
    {
      say "func " ~ $<identifier>.made;
      make $<identifier>.made;
    }
    '(' <parameter> ')' '{' '}'
    { say "end " ~ $/.made; }
  }
  token identifier { \w+ { make ~$/; } }
  token parameter { \w+ { say "param " ~ $/; } }
}

G.parse('sub f ( a ) { }');
```

- `raku` (and the `Language/grammar_tutorial.rakudoc` line-679 example this is
  taken from): `func f` / `param a` / `end f`
- `mutsu`: `param a` / `Use of Nil in string context` / `end ` / `func f`

## Measured characterisation (2026-08-26)

Three variants isolate the trigger exactly.

1. **No `make` anywhere** — same grammar with `{ say "block1" }` /
   `{ say "block2" }` and side-effect-only subrule blocks. mutsu and raku agree
   completely: `identifier`, `block1`, `parameter`, `block2`. Ordering is
   correct when every block runs inline.

2. **`make` in both `function-define` blocks.** raku: `block1 f`,
   `parameter a`, `block2 f`. mutsu: `parameter a`, `block1 f`, `block2` with
   `$/.made` reading `Nil`. Both of the rule's blocks are deferred, so they run
   *after* the child subrule's inline block, and the first block's `make` is not
   visible to the second.

3. **The ticket's own mix** (block 1 has `make`, block 2 does not). mutsu:
   `param a`, `end` (inline, at its textual position), `func f` (deferred). The
   two blocks of one rule end up in *reverse* order because one defers and the
   other does not.

## Root cause

`code_block_defers_to_reduce` (`src/runtime/regex/regex_helpers.rs`) classifies
a block by scanning its source text: a block that mentions the bare identifier
`make`, or any `$*`/`@*`/`%*` dynamic variable, is **deferred**; everything else
runs inline where the cursor reaches it (ADR-0009 part B,
`eval_regex_inline_code`).

A deferred block is stored as a `CodeBlockContext` on the capture node and
replayed later by `reduce_run_code_blocks` /
`execute_regex_code_blocks`(`src/runtime/regex/regex_eval_repeat.rs`) during the
post-order reduce walk over the finished capture tree. That walk visits child
nodes before the parent, which is why `<parameter>`'s block runs before the
parent rule's first block, and the parent node's `ast` (`.made`) is committed
only as the node's reduce step *ends*, which is why a sibling block replayed in
the same step reads `Nil`.

The deferral exists for a real reason: `make` sets `$/.made` on the node being
built, and during matching that node's `Match` does not exist yet — `caps.ast`
is only turned into a Match attribute when the capture tree is reduced. So
"just run it inline" needs somewhere for `make` to write.

## Why this is deep, not a ticket

Running every embedded block inline in match order means:

- **`make` must write into the live capture accumulator.** The inline path
  receives `&RegexCaptures` (read-only by design — see the ADR-0007 delta
  convention in `regex_match_atom.rs`); `make` would need a write channel onto
  the enclosing rule's in-progress node.
- **That write must be undone on backtracking.** Blocks already re-run on
  backtrack in both implementations (measured: `'aaab' ~~ / (\w)+ { … } b /`
  runs its block twice in raku and in mutsu), so an inline `make` will be
  performed on paths that are later abandoned. It needs trail-based undo
  alongside `regex_vars` (`src/runtime/regex/regex_trail.rs`), not a plain
  assignment.
- **`$<child>.made` must already be committed when the parent's block runs.**
  A child subrule's own `make` block has to have run and attached its value to
  the child's node by the time the parent reaches its next atom — i.e. the
  commit point moves from "reduce step" to "subrule match success".
- **The `$*`-dynamic-variable deferral rides the same mechanism.** It was added
  deliberately (`news/2026-08/grammar-token-param-dynvar-not-visible-in-subrule.md`)
  so a block mentioning a `$*` name replays with the bindings that were in force
  at its textual position, carried in `CodeBlockContext.dyn_params`. Moving
  blocks inline removes the need for that carry, but the two changes have to
  land together or per-match `:my $*x` bindings regress.
- **Grammar-action (`:actions`) dispatch legitimately stays at reduce time.**
  Only the *inline block* half should move; the reduce walk must survive for
  action methods, `silent_caps`, and the `.made` propagation an action produces.

## Suggested shape

Keep the reduce walk for `:actions`; make embedded `{ … }` blocks unconditional
inline execution, with `make` writing to a mutable "current node ast" slot in
the engine that the subrule-success path folds into the node it just built, and
the backtrack trail restoring. `code_block_defers_to_reduce` then goes away
entirely, which also removes a text-scanning heuristic that mis-classifies (a
block containing the *word* `make` inside a string literal defers today).

A blast-radius note: every grammar in the batteries (`YAMLish`, `JSON::Fast`'s
grammar path, `Cro::HTTP`, `TOML`, the vendored `zef`) uses `make`, so this
lands under full-roast plus battery coverage or not at all.
