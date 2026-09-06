# Every embedded regex code block now runs in match order, and the reduce-time replay is gone

`news/2026-09/grammar-inline-code-block-order.md` moved every `make`-bearing
embedded `{ … }` block onto the inline (match-order) path and shrank
`code_block_defers_to_reduce` (`src/runtime/regex/regex_helpers.rs`) to exactly
one remaining test: a block mentioning a `$*` / `@*` / `%*` dynamic variable
still deferred to the post-match bottom-up reduce walk. That left the same
ordering bug on a narrower set of blocks — within one rule, a `$*`-mentioning
block ran after the rule's plain blocks and after its subrules' blocks, so two
blocks of one rule executed in reverse source order:

```raku
grammar G {
  token TOP { :my $*N = 0; <a> { $*N = 1; say "dyn" } { say "inline" } <b> }
  token a { \w }
  token b { \w }
}
say G.parse('xy').defined;
```

raku printed `dyn` / `inline`; mutsu printed `inline` / `dyn` (re-measured
2026-09-06, still reproducing).

`code_block_defers_to_reduce` is now gone entirely, and with it the whole
reduce-time replay machinery: `CodeBlockContext`, the `code_blocks` axis on
`RegexCaptures` / `CapNode`, its `Undo::CodeBlocksLen` trail entry, the
`EAGER_CODE_BLOCKS` buffer with `enable_`/`drain_`/`execute_regex_code_blocks`,
`reduce_run_code_blocks`, `setup_regex_code_block_env`, `install_ctx_regex_vars`
/ `restore_ctx_regex_vars`, `block_base_vars`, `subtree_has_code_blocks`, and
the `ACTIVE_DYN_PARAMS` stack that carried a rule's `$*` parameter bindings
alongside a deferred block. The reduce walk is now purely per-rule `:my $*x`
bookkeeping for the `:actions` pass, and it returns immediately for any grammar
that declares no such variable — which is nearly all of them. Net −490 lines.

## Two things rode the deferral; both turned out not to need it

- **`$*` rule parameters.** `token value($*STOPPER = '"')` binds its parameter
  for the duration of the subrule's match by writing `self.env` around it
  (`install_subrule_dynamic_params`). A block that runs *inline* is inside that
  window, so it reads the binding directly and `CodeBlockContext.dyn_params` had
  nothing left to carry. `t/grammar-dynvar-failgoal-ws.t` — the pin for this
  half, 51 assertions including the nested-shadowing and `:args(...)` cases —
  passed unchanged the moment the blocks went inline.
- **Per-match `:my $*x`.** This one did need work, but not the work the ticket
  proposed. The ticket's plan was to snapshot each declaring rule's dynvar keys
  onto its capture node at match success, at four separate subrule-success
  sites. Measuring showed the snapshot already exists: the `:my $*x = …;`
  declarator is a `RegexAtom::VarDecl` that evaluates its initializer per match
  and records the result in the capture delta's `regex_vars`, and an inline
  block's writes are harvested into the same carrier. So the node already holds
  what the variable held when that match ended.

  The only change needed was at the *reading* end: `install_fresh_rule_dynvars`
  re-derived the value at reduce time by re-running the declaration, which threw
  away every write. It now installs the node's recorded value instead, and falls
  back to re-evaluating the declaration only for shapes the carrier never reached.
  `record_rule_dynvars` correspondingly re-reads the env only for those fallback
  keys — reading it back for a key installed *from* the node would let a child's
  leftover binding of the same name overwrite it.

  The outer-declaration accumulation shape (`token TOP { :my %*PLAYED = ();
  <card>+ }`, where the children's *actions* accumulate into the parent's
  binding) keeps working for the same reason it did before: the reduce walk
  deliberately leaves the declaring node's binding installed in the env, the
  children's actions mutate that container, and the node holds the same
  container, so the parent's own action sees the mutations.

## The blocker underneath: an inline write inside a group was silently dropped

Moving the blocks inline immediately failed `t/grammar-per-match-dynvar-action.t`
in a way the ticket had not predicted, and the cause was a **separate,
pre-existing bug** with nothing to do with dynamic variables: a write an inline
`{ … }` block made to any in-regex `:my`/`:let` lexical was lost as soon as the
block sat inside a group.

`RegexCaptures::regex_vars` is the carrier for those writes. The
single-candidate matcher (`regex_match_capture.rs`) propagated it out of a
`Group`; the **plural** matcher (`regex_match_atom.rs`) — the one the main
`walk_tokens` actually uses — did not, and neither did `merge_regex_captures`
(so a conjunction branch and a `~` goal dropped it too) nor the two LTM
ε-bypass folds in `regex_ltm_rank.rs`. The write leaked into `self.env` instead,
where the *next* block promptly overwrote it by installing its own (stale)
`regex_vars` snapshot. So

```raku
grammar G { token part { :my $V = 'decl'; \w [ { $V = 'set' } ] { make $V } } }
```

made `decl` where raku makes `set` — for a plain lexical, with no dynamic
variable anywhere. Six shapes were checked against raku and all six were wrong
in mutsu: a non-capturing group, a capturing group, an alternation branch, an
ordered-alternation branch, two groups deep, and a quantified group. All six
pass now.

## Verified

- The headline repro matches raku exactly.
- New pin `t/grammar-dynvar-code-block-order.t` (12 assertions, every one run
  against real `raku` first): source-order execution of a `$*` block against a
  plain sibling, subrule-before-parent ordering, per-match `$*V` reaching both a
  later block and the match's action, and the six sub-pattern shapes above.
- The named pins all still pass: `t/grammar-per-match-dynvar-action.t`,
  `t/grammar-reduce-time-dynvar.t`, `t/grammar-dynvar-failgoal-ws.t`,
  `t/grammar-inline-code-block-order.t`,
  `t/grammar-inline-make-subrule-made.t`,
  `t/ordered-alternation-loser-code-block.t`.
- `make test` (3733 files / 38583 tests) and `scripts/battery-testsuite.sh` are
  green, plus the full roast suite in CI. The batteries are the load-bearing
  check here: `YAMLish`, `JSON::Fast`, `Cro::HTTP`, `TOML` and the vendored
  `zef` are all grammar-driven.

## Residue (measured, not regressions)

An unordered alternation (`|`) still evaluates every branch eagerly, so a
side-effect-only block in a losing branch runs. That is the pre-existing
`todo/deep/ordered-alternation-eager-candidate-enumeration.md`; the *carrier* now
takes its value from the winning branch, so the value-producing case is right
even where the side effect fires early. Ordered alternation (`||`) is already
driven against the real continuation and is unaffected.
