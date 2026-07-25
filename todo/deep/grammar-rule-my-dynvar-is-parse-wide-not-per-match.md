# A rule's `:my $*FOO` is one parse-wide binding, not one per match, so every action reads the last match's value

Found 2026-07-25 in File::Ignore. Root cause **re-diagnosed 2026-07-25** after an
attempted fix — the earlier diagnosis (two bottom-up passes running in the wrong
order) is where the symptom shows, but it is not where the bug is. Read the
"What was measured" section before starting: two plausible approaches were tried
and both are dead ends for the reason given there.

*(The original PLAN §8.20 — proto-token LTM equal-length tie-break — was FIXED
2026-07-25: the resolver sorted `:sym<>` candidates alphabetically instead of by
declaration order, and the quantified-subrule LTM dedup kept the LAST candidate
on a tie. Fix: stamp `FunctionDef.decl_order` at `insert_token_def`, sort
sym-variant keys by it, keep the first-declared candidate on an equal-length tie
in `regex_match_atom.rs`. Pin: `t/proto-token-ltm-tiebreak.t`. That took
`File::Ignore` `wildcard.rakutest` 36/44 → 38/44. The remaining 6 are THIS bug.)*

## Repro

```raku
grammar G {
    token TOP { <part>+ % '/' }
    token part {
        :my $*FINAL;
        \w+ {}
        [<?before '/'? $> { $*FINAL = True }]?
    }
}
class A {
    method TOP($/) { make $<part>.map(*.ast).join('|') }
    method part($/) { make ($*FINAL ?? "FIN" !! "mid") ~ ":$/" }
}
say G.parse('a/b/c', :actions(A)).ast;
# raku:  mid:a|mid:b|FIN:c      mutsu: FIN:a|FIN:b|FIN:c
```

A smaller one that isolates it from `make`:

```raku
grammar G2 {
    token TOP { <part>+ % ',' }
    token part { :my $*V = 'decl'; \w+ [ <?before ','> { $*V = 'set' } ]? }
}
class A2 { method part($/) { say "part($/) sees: $*V" } }
G2.parse('a,b', :actions(A2));
# raku:  part(a) sees: set / part(b) sees: decl
# mutsu: part(a) sees: set / part(b) sees: set
```

## Root cause (measured, not inferred)

`Interpreter::establish_grammar_dynamic_vars` (`src/runtime/methods_grammar.rs`)
runs **once per parse**. It scans every rule pattern of the grammar (its package,
MRO-walked) for `:my $*/%*/@*NAME = INIT;` substrings, evaluates each one into
`self.env`, and returns the prior values so the parse can restore them at the
end. So `$*FINAL` is a **single parse-wide slot** shared by every match of
`part` — the last code block to write it wins for every reader, whenever they
read.

That the two reduce passes (`reduce_regex_captures_made` for inline `{ … }`
blocks, then `invoke_grammar_actions` for actions) are separate is what makes it
*visible* — all writes finish before any action runs — but even a perfectly
interleaved walk would still be reading one shared slot. Rakudo gives each match
of the declaring rule its own binding.

## What was measured (do not re-try these)

- **`RegexCaptures::regex_vars` is empty on every reduce node.** Instrumented
  `reduce_regex_captures_made` and printed `caps.regex_vars` per node for the
  repro above: `[]` on the `part` nodes and on TOP. The `RegexAtom::VarDecl` arm
  in `regex_match_capture.rs` *does* build a `regex_vars` delta, but it never
  arrives — and adding `regex_vars` to `merge_regex_captures` plus all six
  `code_blocks` delta-copy sites in `regex_match_atom.rs` /
  `regex_match_capture.rs` did **not** change that. Something between the delta
  and the node still drops it; `CapStore::snapshot` is not the culprit (it
  clones the whole struct). Do not build on `regex_vars` without first proving
  it arrives.
- **Carrying a reduce-time snapshot to the action works mechanically.** A
  `reduce_time_vars` attribute set in `make_subcap_match` (which already carries
  `ast` / `action_name` per node) and installed/restored around the action
  dispatch in `invoke_grammar_actions` is a small, clean change — it just has no
  data to carry while the point above holds. The top-level node needs nothing:
  it reduces last and its action runs last, so the shared slot already holds its
  own value.
- **Snapshotting all `*`-prefixed env keys per node instead** was considered and
  rejected: it costs an env scan at every reduce node (grammar-parse perf is a
  tracked campaign), and restoring a mutated container by reference would not
  reproduce raku for the accumulate-across-matches shape
  (`:my %*PLAYED = ()` mutated by child code blocks) anyway.

## Fix direction

Make the binding per match of the declaring rule instead of per parse. The
declarations are already known statically per rule pattern
(`collect_dynamic_var_decls` / `dynamic_decl_var_key`), so the material is
there; what is missing is (a) knowing, at a reduce node, which rule it came from
(`caps.action_name` carries it only for aliases — the name otherwise lives on
the *parent's* `named_subcaps` key), and (b) a place to hold each match's value
until its action runs. `establish_grammar_dynamic_vars` should probably keep
providing the parse-wide *initial* value, with each match of a declaring rule
saving/restoring its own around its reduce and its action.

## Affected files

- `src/runtime/methods_grammar.rs` — `establish_grammar_dynamic_vars`,
  `collect_dynamic_var_decls`, `dynamic_decl_var_key`, `invoke_grammar_actions`.
- `src/runtime/regex/regex_eval_repeat.rs` — `reduce_regex_captures_made`.
- `src/runtime/regex/regex_match_capture.rs` — the `RegexAtom::VarDecl` arm.
- `src/value/value_methods_c.rs` — `make_subcap_match`, if a per-node value is
  carried onto the Match.

## Already fixed, for context

The **failed-parse half** of the original report is done (2026-07-25): when the
overall parse FAILS, mutsu used to run no actions at all. The matcher now logs
every named-subrule reduce (`REDUCED_SUBRULES` in `regex/regex_helpers.rs`),
`.parse` keeps the longest partial match instead of discarding it, and the
failure path dispatches that partial tree plus the reduces that fell outside it.
That took HTTP::UserAgent's upstream suite 23/27 → 25/27. Pin:
`t/grammar-actions-on-failed-parse.t`.

## Impact

The 6 remaining `File::Ignore` `wildcard.rakutest` `a/**/b` failures. The
mid-path globstar's compiled regex is correct (a direct
`/^ 'a' '/' [ <-[/]>+ [ '/' | $ ] ]* 'b' <?before "/" | $> /` matches in mutsu);
the module builds a *wrong* pattern string (`'a'` missing its trailing ` '/'`)
because the `a` segment's action wrongly sees `$*FINAL = True`.
