# A repeated multi call goes straight to its winner

A bare `multi` call whose winner is a function of its argument types now skips
the generic dispatch once those types were seen. A multi name stays out of the
name-keyed light-call caches (one entry would serve every argument shape), so
every call used to normalize its arguments, resolve the winner, probe the
compiled-body table and ask a dozen "is this a multi / proto / declared"
memos before reaching the same positional light bind a plain sub uses.

`multi_light_cache` keys the whole dispatch by (package, name, argument type
keys), the key the winner cache already used. The generic path fills it, and
a later call with the same types goes from `CallFunc` straight to the winner's
light bind, with the dispatch frame `nextsame`/`callsame` and `samewith` read.
It re-checks per call only what can differ per call (the bind's admissions, a
wrapper). A registry change, an inner scope's candidate or a `wrap` retires
the entries.

The #10109 repro (`multi kind(Int $x)` / `multi kind(Str $x)` called 3000
times) went from 13.3k to 8.7k instructions per call; the same call of a
plain `sub kind(Int $x)` is 9.8k. This is the call-site-cache half of
ADR-12529 §2.4 (#10109).
