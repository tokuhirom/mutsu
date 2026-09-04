# A `my`-scoped proto keeps its whole multi lexical

The PROTO's scope decides a multi's visibility, not the candidates':

```
raku  -e 'module M { proto sub f($) {*}; our multi sub f(Int $x) { "f" } }; say M::f(1)'
Could not find symbol '&f' in 'M'
mutsu -e '...same...'   # before: f
```

## Two halves, and the second is what made the first come out wrong

**The marker.** mutsu marks a package key `my`-scoped when a plain `sub`
registers (so `module M { sub j($) {…} }` already answered "Could not find
symbol"), and `our`-scoped when an `our proto` registers — the latter
deliberately overriding the candidates, because a bare `multi sub f(...)`
candidate would otherwise mark `Pkg::f` lexical and hide a routine the `our
proto` published. The negative half was missing: a BARE proto marked the key
neither way, and a candidate that says `our` does not mark it either, so the key
stayed unmarked and the qualified call resolved. `register_proto_decl` now marks
it `my`-scoped when the proto is not `our`, the mirror image of the `is_our`
branch beside it.

**The probe.** With the marker in place the refusal came out as

```
Cannot resolve caller M::f(Int:D); none of these signatures matches:
    (Int $x)
```

`resolve_proto_function` and `resolve_function_with_types` both consult
`qualified_name_hidden_here`, so they correctly answered `None` — but
`Interpreter::has_proto`, the probe that decides whether to build a
no-matching-candidate error, did not. So the call refused for the wrong reason,
and printed the signatures of a routine the caller is not allowed to see.
`has_proto` now applies the same gate.

## What stays visible

`our proto sub g` publishes the multi whether its candidates are `our` or bare;
an `our sub` is published; and the lexical short name still dispatches from
inside its own package (`our sub call-q($v) { q($v) }` reaches both candidates
of a bare `proto sub q`), which is the whole point of the routine being lexical
rather than absent. A mainline `proto`/`multi` is unaffected.

## Coverage

`t/my-scoped-proto-visibility.t` — 12 assertions, all dual-oracled against
rakudo: the hidden shapes with rakudo's message, the published shapes, the
inside-the-package dispatch, and the mainline control. `make test` (3648 files)
and a full local `make roast` (1436 files, 218962 tests) are green.

## Provenance

Split out of `news/2026-09/our-multi-in-a-package-body.md` the same day: before
that change mutsu rejected `module M { proto …; our multi … }` outright, so the
lookup was unreachable. Accepting the declaration (as rakudo does) exposed this.
