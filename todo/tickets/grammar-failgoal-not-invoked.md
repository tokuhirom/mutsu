# A grammar's `FAILGOAL` method isn't invoked when a goal-matching conjunction (`~`) fails

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
2892).

## Repro

```
grammar A {
    token TOP { '[' ~ ']' \w+ };
    method FAILGOAL($goal) {
        die "Cannot find $goal near position {self.pos}"
    }
}
say A.parse: '[good]';
A.parse: '[bad';
CATCH { default { put .^name, ': ', .Str } };
```

- raku: `｢[good]｣` then (on the failing parse) `X::AdHoc: Cannot find ']'  near position 4`
- mutsu: `｢[good]｣` then nothing — the failing `A.parse: '[bad'` produces no output and no
  exception is caught (the `CATCH` block presumably never fires because nothing was thrown)

`'[' ~ ']' \w+` is the goal-matching conjunction form (`OPENER ~ CLOSER MIDDLE`): when the
closer `]` can't be found, Raku is supposed to call the grammar's `FAILGOAL` method (if defined)
instead of just failing silently, so the user can customize the failure (here, `die`-ing with a
custom message).

## Root cause guess

The goal-matching conjunction operator (`~` inside a regex/token) likely already handles the
successful-match case, but doesn't check for (or call) a user-defined `FAILGOAL` method on parse
failure — it probably just returns/fails silently the way a normal non-conjunction match failure
would.

## Affected files (starting point)

- `src/runtime/regex.rs` — goal-matching conjunction (`~`) implementation, grammar method
  dispatch on match failure
