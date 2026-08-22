# `<sym>` named-capture inside a `token X:sym<...>` multi-token body isn't recorded on the Match

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/grammars.rakudoc:260`).

## Root cause hypothesis

When a `proto token`/multi-dispatch token body (`token letter:sym<R> { <sym> }`) matches
via the special `<sym>` regex atom (which captures the literal `sym<...>` string used for
that candidate), raku records that capture into the resulting `Match`'s named-capture hash
(`$/.hash` includes `sym => Match.new(...)`), so `$match<sym>` is truthy for every match
produced by a `sym<...>`-carrying candidate (and falsy/absent for a catch-all
`token letter:sym<*> { . }` candidate that never mentions `<sym>`). mutsu's `<sym>` atom
appears to match successfully (the token still matches the right characters — `.raku`
shows the correct `:from`/`:pos` — and the doc's overall filter-by-`.grep(*.<sym>)`
approach *should* separate matched-letters from the catch-all), but the resulting `Match`
objects never get a `sym` entry in their hash at all, for any candidate, so
`.grep(*.<sym>)` finds nothing and the actions method's join is empty.

## Minimal repro

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
    method TOP($/) { say $<letter>.raku; make $<letter>.grep(*.<sym>).join }
}).made.say;
```

- `raku`: each matched-letter `Match` object's `.raku` shows a trailing
  `:hash(Map.new((:sym(Match.new(...)))))` for the `R`/`a`/`k`/`u` candidates (and none for
  the `sym<*>` catch-all); `.grep(*.<sym>)` correctly keeps only those, `.join` → `Raku`.
- `mutsu`: none of the `Match` objects carry any `:hash(...)` in their `.raku` — the `<sym>`
  capture is entirely missing — so `.grep(*.<sym>)` returns nothing and `.made` is `""`
  (empty line printed, not `Raku`).

## Affected files (starting point)

- `src/runtime/regex/regex_eval*.rs`, `src/runtime/regex/regex_eval_repeat.rs` — wherever
  the `<sym>` builtin regex atom is matched during proto/multi-token dispatch; needs to
  register a `sym` entry into the resulting `CapNode`/Match hash the same way an explicit
  `$<name>=...` named capture does.
- Grep for how `sym<...>` candidate dispatch is compiled/executed (likely
  `runtime/methods_grammar.rs` or the proto-dispatch machinery) to find where the matched
  candidate's `sym` value is known but not threaded into the capture tree.
