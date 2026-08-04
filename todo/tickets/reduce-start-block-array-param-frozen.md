# A `start` block inside a `reduce` callback sees the FIRST iteration's `@`-param forever

The `start` closure spawned from a `reduce` callback captures the callback's
`@`-sigil parameter frozen at its first binding — every later iteration's (and
every later *call*'s) spawn still computes with the first values:

```raku
say reduce -> $h, @words { $h + await start { [+] @words } }, 0, (1,2), (3,4);
# raku:  10   (3 from (1,2), then 7 from (3,4))
# mutsu:  6   (3 + 3 — the second iteration's start saw the FIRST @words)
```

The freeze is specific to this combination — each neighbouring shape is
correct:

- `reduce -> $h, @words { $h + [+] @words }, ...` (no `start`) — correct;
- `map -> @words { await start { [+] @words } }, [1,2], [3,4]` — correct;
- a direct block call twice (`-> @words { await start { … } }`) — correct;
- a `$`-sigil param in the same reduce+start shape — correct.

This is the same family as the fixed
`news/2026-08/start-block-destructured-array-param.md` (a destructured `@`
param frozen at the first spawn's value on the shared-var name lane), but for a
plain `@` parameter re-bound across reduce iterations.

## Why it matters — `Digest::RIPEMD` is still wrong for any input after the first

`rmd160`'s compression loop is exactly this shape (`reduce -> blob32 $h,
@words { … |await map -> [...] { start { … @words[…] … } } … }`), so:

- any **multi-block** message (>55 bytes) digests wrongly even on the FIRST
  call — the second 16-word chunk is processed with the first chunk's `@words`
  (`rmd160("abcdbcde…nopq")` gives `8e0c0aa0…` instead of `12a053384a9c…`);
- any call **after the first in one process** returns the FIRST call's digest
  regardless of input (`rmd160("abc"); rmd160("")` returns abc's digest twice).

With the anonymous-state fix (`news/2026-08/anon-state-per-routine-call.md`)
the rotate counter is correct, so repeated same-single-block calls now agree;
this freeze is the whole remaining wrongness in
`todo/tickets/digest-dist-blockers.md` blocker 2, and it blocks the dist's
`t/ripemd.t` (whose RFC vectors include 2-block and 1M-byte messages).

Reproduced on main 2026-08-04 (predates the anonymous-state work; both debug
and release). Suspected area: the `start`-spawn env snapshot / shared-var name
lane for block parameters (`src/runtime/` start handling), where the reduce
callback's re-binding of `@words` on later iterations does not reach the
already-established shared name.
