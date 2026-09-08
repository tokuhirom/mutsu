# A `Code` object inside a container renders as itself

`say (&b,)` printed `()`. The element was there — `.elems` was 1 — but it
rendered as the empty string, so a one-element list was indistinguishable from
an empty one, which changes how many elements the reader believes the list has.

```raku
my &b = { $^a };
say (&b,);        # mutsu ()   raku (-> $a { #`(Block|…) ... })
say (&b,).elems;  # 1 in BOTH -- the element exists
```

The pure container renderers (`gist_value`, `raku_value`) have no `Code` arm, so
a Code element fell through to `to_string_value()` — which is Code's *`.Str`*
rule, the bare name. That is empty for an anonymous block, and for a named
routine it drops the sigil, so `(&f,)` rendered as `(f)`.

The correct `Code.gist` / `Code.raku` already existed in the `Sub` method
handler. Rather than grow a second implementation of them, Code leaves now take
the interpreter-dispatch route the container renderers already used for an
instance carrying a user-defined `method gist`. Three separate gates decide that
route and all three needed the same arm — fixing one at a time was misleading,
because adding it to the interpreter-side walk alone fixed `(&f,)` while leaving
`(&b,)` empty:

- `methods_call_dispatch`'s local `collection_contains_instance` — the
  interpreter-side list walk;
- `utils::gist::contains_instance_seen` — the VM's collection-bypass probe,
  which is what actually decides whether the native fast path is used at all;
- `raku_repr::needs_raku_dispatch` — the `.raku` leaf probe, the same hole on
  the `.raku` side (`(&b,).raku` was `(,)`).

Two rendering details came with it. A `Block` writes its signature *bare* after
the arrow — `-> $a { ... }` — where a `Sub`'s `.raku` keeps the parens
(`sub ($a) { ... }`); `sig_gist` is the Signature's own parenthesized gist, so
the Block form strips one layer.

And a placeholder block `{ $^a }` is a `Block` in raku, not an anonymous `Sub`.
mutsu classified it as a Sub, so it gisted as `sub { }` with its signature
dropped entirely. The AST already recorded the source spelling on
`AnonSubParams::is_sub` ("true only when the `sub` declarator was written"), but
the compiler discarded it (`is_sub: _`) and guessed blockness from a heuristic
instead — "a pointy block injects a `SetLine` as its first body statement" —
which a placeholder block does not match. The flag is threaded through now; the
heuristic remains as the fallback for the closures the compiler and runtime
synthesize, which have no source spelling to record. The `^` twigil is
declaration syntax rather than part of the parameter's name, so it no longer
appears in the rendered signature.

All nine shapes in the ticket now match rakudo v2026.07 exactly, modulo the
`#`(Block|N)` address that no run can reproduce.
`t/code-object-gist-in-container.t` therefore asserts the *shape* — non-empty,
starts with `-> $a`, carries the declarator comment — and passes unchanged on
rakudo.

One claim in the ticket did not survive measurement: its third listed defect, a
`Method`'s gist being just its name, does **not** reproduce on rakudo v2026.07.
`Str.^lookup('Int').gist` is `Int` there too, matching mutsu, so nothing was
changed for it.

Closes #7587.
