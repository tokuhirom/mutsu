# `Supply.head`/`unique`/`lines`/`words`/`elems` are real pipeline stages now

A live `Supply` combinator is supposed to allocate a derived supplier and
register a tap on its source that forwards into it — `map`, `grep`, `flat`,
`batch`, `reduce`, `migrate` and (as of #7995) `produce` all do that. Five
combinators did not: `head`, `unique`, `lines`, `words` and `elems` handed
back a `Supply` carrying the *source's* `supplier_id` plus a marker
attribute (`head_limit`, `unique_filter`, `is_lines`, `is_words`,
`elems_filter`), consulted only when the user eventually tapped it.

That worked for a single stage and when such a combinator was *last* in a
chain, but broke the moment another combinator was chained after one: the
next combinator read `supplier_id` off the attributes, registered itself on
the shared source id, and the marker attribute was simply dropped — the
stage vanished.

```raku
my $s = Supplier.new;
my @a;
$s.Supply.head(2).map(* * 10).tap(-> $v { @a.push($v) });
my @b;
$s.Supply.unique.map(* ~ '!').tap(-> $v { @b.push($v) });
$s.emit($_) for 1, 1, 2, 3;
say @a;   # rakudo: [10, 10]           mutsu (before): [10, 10, 20, 30]
say @b;   # rakudo: ["1!", "2!", "3!"] mutsu (before): ["1!", "1!", "2!", "3!"]
```

Fixed the same way `produce` was in #7995: each of the five now allocates
its own derived supplier at combinator-call time and registers a transform
tap on the source immediately, instead of deferring registration to
whenever `.tap()` eventually runs.

- `head` forwards each value under its limit to its own derived supplier,
  and reaching the limit now fires *that* supplier's own done — not the
  source's, since the source may still have more values coming and other
  stages chained off it must keep seeing them.
- `unique`/`elems` forward the same way, replacing their "call the tap
  callback directly" action with a plain forward.
- `lines`/`words` forward each split line/word, and the trailing partial
  buffer is flushed into their derived supplier (not the tap callback) at
  `done`, exactly like a batch buffer already was.
- The `"tap"|"act"` chokepoint's marker-checking `else if` chain for these
  five is gone: a Supply reaching it with a genuine `supplier_id` is, by
  construction, just an ordinary live supply by then.
- `.live` is `False` for all five over a live source, matching `produce`/
  `batch` (and unlike `map`/`grep`, which stay live) — confirmed against
  `raku` directly; getting this wrong regressed
  `t/concurrency/supply/supply-unique-tap-ok-expires.t`'s `tap-ok` "Supply
  appears to NOT be live" assertion during development.

A channel-backed live Supply (`Supply.interval` without `:scheduler`,
`Proc::Async` output, an async socket) is unaffected: it has no
`supplier_id` at all, so `head`/`lines`/`words` fall through to their
existing marker-forwarding path for that case, which the "act loop pump"
still consults exactly as before.

`t/concurrency/supply/supply-combinator-chain.t` gains 17 new assertions:
each of the five chained both directions (before and after `map`), `done`
propagating through each (including the lines/words trailing-partial
flush), a sibling stage on the same source still seeing every value after
`head` stops at its own limit, and the `.live` check — all verified against
`raku` directly.

Closes #8474.
