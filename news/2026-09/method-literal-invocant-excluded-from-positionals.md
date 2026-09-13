# Pinning the half of #8311 the method-literal fix left untested

[#8311](https://github.com/tokuhirom/mutsu/issues/8311) reported two divergences for a nameless
method literal: the type it reports (`Sub` / `Block` instead of `Method`), and the invocant not
being excluded from the signature's positionals. Both were fixed by
`8063666da` ("a method literal is a Routine, not a Block", written against the
MetamodelX::Dataclass parity cluster rather than against this ticket), and that commit's pin
`t/oo/method/method-literal-is-a-routine.t` covers the first divergence thoroughly — every
declarator spelling, `return` acting as a routine boundary, and the types that must not have moved.

It does not cover the second. Its invocant tests are a user-named invocant (`anon method ($obj: $x)`)
and the Dataclass site's `Mu $obj: *%args`; neither is the shape the ticket documents, which is a
**type-object invocant marker in front of a positional slurpy**:

```
$ raku -e 'class W {}; sub c(&r) { r(W, 1, 2) }; say c(anon method (W: *@a) { @a.join(",") })'
1,2
```

The slurpy is what makes the bug observable at all. A fixed-arity signature fails loudly on the
arity when it is handed one argument too many; a slurpy just quietly binds the extra one, so the old
behaviour showed up only as a stray leading comma (`W.join` stringifies to empty). The `::` null
declarator name from [#8294](https://github.com/tokuhirom/mutsu/issues/8294) — the spelling
`UI::HTMLWindow` actually uses, `&routine.wrap(anon method :: (Window: *@_, *%_) { ... })` — was
likewise unpinned.

`t/oo/method/method-literal-invocant-excluded-from-positionals.t` adds 10 tests for exactly that gap, with no
overlap against the existing file: the `W:` marker in front of `*@a` across the `method`,
`submethod`, `anon method` and `anon method ::` spellings; a named slurpy beside a positional one;
ordinary positionals binding from the *second* argument on; `self` being the receiver rather than
the first positional; and the neighbouring shape that must keep the old behaviour — a `sub` literal,
which has no invocant concept, still slurping all three arguments. Green under mutsu and under real
Rakudo.
