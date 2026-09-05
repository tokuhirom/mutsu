# A `$*`-mentioning regex code block still runs at reduce time, not in match order

`news/2026-09/grammar-inline-code-block-order.md` moved every `make`-bearing
embedded `{ … }` block onto the inline (match-order) path and shrank
`code_block_defers_to_reduce` (`src/runtime/regex/regex_helpers.rs`) to exactly
one remaining test: a block that mentions a `$*` / `@*` / `%*` dynamic variable
is still deferred to the post-match bottom-up reduce walk.

That leaves the *same* ordering bug the `make` half had, on a narrower set of
blocks: within one rule, a `$*`-mentioning block runs after the rule's inline
blocks and after its subrules' blocks, so two blocks of one rule can still
execute in reverse source order when one of them names a dynamic variable and
the other does not.

## Repro (measured 2026-09-05)

```raku
grammar G {
  token TOP { :my $*N = 0; <a> { $*N = 1; say "dyn" } { say "inline" } <b> }
  token a { \w }
  token b { \w }
}
say G.parse('xy').defined;
```

- raku: `dyn` / `inline` / `True`
- mutsu: `inline` / `dyn` / `True`

The two blocks of one rule run in reverse source order, because the first
mentions `$*N` and defers while the second runs inline. Re-measure before
designing, per the project's standing rule.

## Why the deferral exists

Two things ride it, both added deliberately:

- **Per-match `:my $*x`.** `install_fresh_rule_dynvars` re-evaluates a rule's
  `:my $*x = …;` declarations at the *start* of that node's reduce step, the
  node's deferred blocks then run and write the variable, and
  `record_rule_dynvars` copies the result onto the node so the node's **action
  method** re-installs its own match's value. Pinned by
  `t/grammar-per-match-dynvar-action.t` and `t/grammar-reduce-time-dynvar.t`.
- **`$*` rule parameters.** `token value($*STOPPER = '"')` binds its parameter for
  the duration of the subrule invocation only, so a deferred block carries the
  bindings that were live at its textual position in `CodeBlockContext.dyn_params`
  and they are reinstalled around the replay
  (`news/2026-08/grammar-token-param-dynvar-not-visible-in-subrule.md`,
  `src/runtime/regex/regex_dynparams.rs`). Pinned by
  `t/grammar-dynvar-failgoal-ws.t`.

## Why it is deep, not a ticket

Running these blocks inline removes the need for the `dyn_params` carry outright
(the bindings are live in `self.env` at that moment — measured: an inline
`<?{ … $*V … }>` assertion already reads a rule's `:my $*V` correctly, and mutsu
already re-initialises the declaration per match *during* matching, so the value
a block would write inline is the right per-match value). But the *action* half
does not follow for free: `record_rule_dynvars` currently derives the value by
re-running the declaration and the blocks at reduce time. With the blocks gone
from that walk it would record the freshly-declared value and lose every write,
regressing `t/grammar-per-match-dynvar-action.t`.

So the work is: snapshot each declaring rule's dynvar keys onto its capture node
at **match success** (the subrule-success sites in `regex_match_atom.rs`,
`regex_match_capture.rs`, `regex_match_atom_simple.rs` and the quantified-subrule
loop in `regex_match_core.rs`), have the action walk re-install from that
snapshot, and then stop `install_fresh_rule_dynvars` from re-deriving it —
without breaking the outer-declaration accumulation case (`G4` in
`t/grammar-per-match-dynvar-action.t`: `token TOP { :my %*PLAYED = (); <card>+ }`
where the *actions* of the children accumulate into the parent's binding).

Once it lands, `code_block_defers_to_reduce` disappears entirely and the
reduce walk is purely `:actions` dispatch.

## Blast radius

Same as the `make` half: every grammar in the batteries (`YAMLish`,
`JSON::Fast`, `Cro::HTTP`, `TOML`, the vendored `zef`). It lands under full-roast
plus `scripts/battery-testsuite.sh` or not at all.
