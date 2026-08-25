# `<sym>` captures reach the match hash of a `proto token` candidate

A `proto token` whose candidates capture `<sym>` used to produce `Match` objects
with no `:hash(...)` at all, so the canonical Rakudo idiom for filtering the
matched candidates,

```raku
grammar Foo {
    token TOP { <letter>+ }
    proto token letter {*}
          token letter:sym<R> { <sym> }
          token letter:sym<a> { <sym> }
          token letter:sym<k> { <sym> }
          token letter:sym<u> { <sym> }
          token letter:sym<*> {   .   }
}.parse("I ♥ Raku", actions => class {
    method TOP($/) { make $<letter>.grep(*.<sym>).join }
}).made.say;
```

returned nothing: `.grep(*.<sym>)` kept no elements and `.made` was the empty
string instead of `Raku`.

This was filed as a ticket after the doc-diff harness hit it, but it was in fact
already fixed by `bda920728` ("grammar: support proto :sym captures in rules"),
which is pinned by `t/grammar-proto-sym-captures.t`. Re-running the ticket's own
repro verbatim against current `main` now prints `Raku` under both `raku` and
`mutsu`, and each matched candidate's `.raku` carries the expected
`:hash(Map.new((:sym(Match.new(...)))))` — with no such hash on the `sym<*>`
catch-all, matching Rakudo. The ticket was simply never removed when the fix
landed; this closes the bookkeeping.
