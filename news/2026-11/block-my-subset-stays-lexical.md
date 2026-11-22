# A block's `my subset` no longer replaces an outer subset of the same name

`subset S of Int; { my subset S of Str; }; say 3 ~~ S` printed `False`, and
so did the same after a called closure, a `.map` callback, a loop body or an
`if` branch that declared the `my subset`. rakudo prints `True`: the
block's subset is lexical to it (#18217).

- **The bare registry key.** A `my subset` already had its own storage name
  (`S\0<decl-id>`, ADR-0047), but it also wrote its spelling's registry key,
  over the outer subset. It now files that key only when no other subset
  holds it.
- **The binding goes with its scope.** It enrolls its spelling as a block
  declaration (`register_lexical_class`), so the scope's exit reverts the
  binding and suppresses the name, as for a `my class`. A branch or loop body
  that declares one gets the same scope exit as for a `my class`
  (`declares_block_local`). An inlined `.map` callback puts the binding back
  through `my_declared_sym`, as for a `my enum`.
- **The smartmatch type lane.** `$x ~~ S` served a bare word's site memo
  without the in-scope check `GetBareWord` itself makes for a block's
  lexical type (`lexical_memo_still_in_scope`). After a loop body read `S`,
  the first `3 ~~ S` behind the loop still answered the loop's subset.
