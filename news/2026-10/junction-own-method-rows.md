# A junction's own methods follow Rakudo

A method called on a junction now autothreads exactly where Rakudo's does
(ADR-11276 §9.151). Several answers change:

- `any(1, 2).isa(Int)`, `.does(Int)` and `.can('uc')` ask the junction
  itself, as `Mu` does.
- `.not` collapses to one `Bool`.
- `.sink` is `Nil`.
- `.Str` threads: `any(1, "a").Str` is `any("1", "a")`.

`gist`, `raku`, `Str` and `sink` are `Junction`'s method-table rows. The
interpreter and the VM's two method-call paths used to keep three different
lists of the methods a junction does not thread; they now share one rule.
