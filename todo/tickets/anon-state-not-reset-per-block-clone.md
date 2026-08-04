# An anonymous `$` state variable is not reset per block clone

`news/2026-08/anon-state-per-routine-call.md` (#5892) resets an anonymous `$`
state variable when its enclosing *routine* is re-entered. But Raku's unit of
state persistence is the closure **clone**, and re-executing any enclosing
scope re-clones the block literals inside it — so an inner block's anonymous
state must also reset when the enclosing *block* (a `for` body, a `reduce`
callback) is re-entered, with no routine call anywhere:

```raku
for ^2 { say (map { $_ + ++$ }, 10, 20).join(",") }
# raku:  11,22 / 11,22      mutsu: 11,22 / 13,24

say reduce -> $a, $x { $a ~ "|" ~ (map { ++$ }, ^3).join(",") }, "", 1, 2;
# raku:  |1,2,3|1,2,3       mutsu: |1,2,3|4,5,6
```

Named `state` in the identical shape is already correct
(`for ^2 { say (map { state $n; ++$n }, ^3).join(",") }` → `1,2,3` twice), and
a `state` directly in a loop body correctly persists across iterations
(`for ^3 { state $n; $n++; say $n }` → 1 2 3) — so the per-clone bookkeeping
exists for named state; the parser-minted `__ANON_STATE_*` cells just don't
participate in it. The fix is likely to route anonymous state through whatever
mechanism gives named `state` its per-clone identity in this shape, rather
than a new reset pass.

## Consequence

The last wrongness in `Digest::RIPEMD` (`todo/tickets/digest-dist-blockers.md`
blocker 2): the output stage `map {$_[[^5].rotate(++$)]}` runs once per
compression block inside the `reduce` callback, so a multi-block message
(>55 bytes) leaves the counter at 3 when the second block starts and the five
hash words come out rotated — `rmd160("abcdbcde…nopq")` returns the correct
bytes rotated by two words (`27dcf49ada62eb2b12a05338…` instead of
`12a053384a9c…27dcf49ada62eb2b`). Single-block vectors and repeated calls are
correct. Fixing this takes the dist's `t/ripemd.t` RFC vectors to a full pass
(its `'a' x 1_000_000` vector needs a release binary to beat the timeout).

## Minimal repro

Either line above; deterministic, no threading involved.
