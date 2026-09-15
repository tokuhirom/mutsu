# A `Regex` answers `.signature`, `.arity` and `.count`

A Raku `Regex` is a `Method`, so it answers the whole `Routine` introspection surface. mutsu
answered none of it — not even for a plain `rx//`, which raised `No such method 'signature' for
invocant of type 'Regex'`. The same for `.arity` and `.count`.

The interesting part is that the right answer is never just the parameters the declarator wrote.
Every regex signature carries the cursor invocant (`Mu $::`) and the implicit `*%_` that every
method signature gets, so a `rx/a/` that declares nothing at all still has a two-element signature
and an arity of 1:

```
$ raku -e 'say rx/a/.signature.raku'
:(Mu $:: *%_)
$ raku -e 'my $t = token ($x) { \d+ }; say $t.signature.raku'
:(Mu $:: $x, *%_)
```

The *declared* half was already reachable from both of the shapes a regex takes in mutsu. An
anonymous declarator's parameters ride on the value itself — `RegexClosure::signature`, reachable
as `Value::regex_signature()`, added when anonymous declarator signatures landed
([#8293](https://github.com/tokuhirom/mutsu/issues/8293)). A grammar `token`/`rule`/`regex`'s live
on its `Registry::token_defs` `FunctionDef`, which the `.^lookup` path was already reading for
`Code.line`/`Code.file`. What was missing was the synthesized half and a dispatch entry point.

`src/runtime/methods_regex_routine.rs` now builds that parameter list once — invocant, then the
declared parameters, then `*%_` — and reuses `effective_method_param_defs` for the last step, so a
declarator that writes its own `*%foo` suppresses the synthesized slurpy exactly the way a method
declaration does. Two entry points read it:

- A `Regex` **value** — `/.../`, `rx/.../`, and an anonymous `token`/`rule`/`regex` term — answers
  `.signature`, `.arity` and `.count` from `dispatch_instance_and_fallback`, with `Mu` in the
  invocant slot.
- A grammar token reached through the MOP builds its `.signature` from the declared parameters
  instead of the raw-capture placeholder every genuinely-native method falls back to, with the
  declaring grammar in the invocant slot. `G.^lookup('foo').signature` was `:(G $:: |)` where
  rakudo answers `:(G $:: $x, *%_)`.

Because the parameters are the real ones, `.arity` and `.count` come apart the way they do for any
routine: `token ($x, $y?) { … }` has arity 2 and count 3, and `token (*@rest) { … }` has count
`Inf`.

One adjacent gap closed along the way: `.line`/`.file` were answered for a `/.../` literal but
raised `No such method` for `rx/.../`, which is the same value by a different internal
representation. Both now answer the same `Nil` — mutsu records no declaration site for a regex
literal, and `Nil` is the honest answer rather than a fabricated one.

Pinned by `t/regex/regex-routine-introspection.t`, whose 23 assertions were each checked against
rakudo's own output.

Two things found alongside and deliberately left out of this change, each with its own issue:

- A `Method` **object** — the `Instance` that `.^lookup`/`.^find_method` hands back — answers
  `.signature` but not `.arity`/`.count`, for regexes and ordinary methods alike:
  `class B { method foo($a) {} }; B.^lookup('foo').arity` raises `No such method`. That is a
  `Method`-shape gap rather than a `Regex` one
  ([#8416](https://github.com/tokuhirom/mutsu/issues/8416)).
- The `Signature` a `Regex` value answers is rebuilt on every read, so
  `$t.signature === $t.signature` is `False` where rakudo says `True`. That is not new here: a
  name-based `Routine` handle has always behaved the same way (`&say.signature === &say.signature`
  is `False` too), while the `Sub` and `.^lookup`-`Method` paths cache correctly. Fixing it is not
  the missing cache lookup but the *key* — the existing `SubSignatureKey` deliberately holds an
  `Arc` rather than an address, and neither of the leaking shapes has one to reuse
  ([#8417](https://github.com/tokuhirom/mutsu/issues/8417)).

Closes [#8318](https://github.com/tokuhirom/mutsu/issues/8318).
