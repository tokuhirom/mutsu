# An embedded regex block's `my` declaration is lost on a backtracked re-run once the block reads `$/`

A `{ … }` block inside a regex re-runs when the engine backtracks into it (raku
does the same). If such a block declares a lexical with `my` **and** reads the
cursor (`$/` or `$¢`), the declaration silently fails to take effect on the
second and later runs: the variable reads as `Any`.

## Repro

```
$ raku  -e 'if "aaab" ~~ / (\w)+ { my $v = "Q"; my $p = $/.pos; say "v=", $v } b / {}'
v=Q
v=Q

$ mutsu -e 'if "aaab" ~~ / (\w)+ { my $v = "Q"; my $p = $/.pos; say "v=", $v } b / {}'
v=Q
v=(Any)
```

Drop the cursor read and it is correct on both runs:

```
$ mutsu -e 'if "aaab" ~~ / (\w)+ { my $v = "Q"; my $p = 1; say "v=", $v } b / {}'
v=Q
v=Q
```

`$¢.pos` behaves identically to `$/.pos`. The knock-on effect is that a `make`
depending on such a variable produces `Any`:

```
$ mutsu -e 'if "aaab" ~~ / (\w)+ { my $v = "Q"; my $p = $/.pos; make $v } b / { say $/.made.raku }'
Any        # raku: "Q"
```

## Where to look

`Interpreter::eval_regex_inline_code` (`src/runtime/regex/regex_eval.rs`) scopes
the block's own `my` declarations by collecting their names into `scoped`,
snapshotting `self.env` for those names, and restoring afterwards. Reading the
cursor forces/materialises the `$/` Match, which makes
`eval_regex_code_block_body`
(`src/runtime/regex/regex_eval_repeat.rs`) see the `"/"` / `"¢"` bindings as
*rebound* and push them — along with the block's `my` name — into
`pending_local_updates`, which the VM writes back into the caller's compiled
local slots. The hypothesis to test first is that the writeback installs a
caller-level slot for the block-local name, which then shadows or defeats the
`my` on the next run.

## Why it is a separate finding

It has nothing to do with `make` or with the reduce-time/inline split: the block
in the repro contains neither `make` nor a `$*` dynamic variable, so it already
took the inline path before `news/2026-09/grammar-inline-code-block-order.md`
moved `make` onto it. That work only surfaced it, while measuring the
backtracked-re-run cases.

## Suggested fix shape

Either keep the block's `my` names out of `pending_local_updates` entirely (they
are lexical to the block, exactly like the regex's own `:my` lexicals which are
already filtered there), or make the cursor bindings not count as "rebound" when
the only change is materialisation of a lazy Match.
