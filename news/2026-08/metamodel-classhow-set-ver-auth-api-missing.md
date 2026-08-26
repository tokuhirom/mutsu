# `Metamodel::Versioning`'s write side: `.^set_ver` / `.^set_auth` / `.^set_api`

`Perl6::Metamodel::ClassHOW` had the read half of `Metamodel::Versioning`
(`.^ver`/`.^auth`/`.^api`, fed by the declarative `class C:ver<1.0>:auth<foo>:api<2>`
adverbs) but none of the mutators, so the documented
`BEGIN { Versioned.^set_ver: v0.0.1 }` idiom from `Type/Metamodel/Versioning.rakudoc`
died with `No such method 'set_ver'`.

## What was already right

The ticket flagged the read side as "presumably equally unimplemented or, at best,
partially implemented" — measurement said otherwise. `.^ver`/`.^auth`/`.^api` already
matched Rakudo exactly, including the non-obvious defaults: an undeclared `.^ver` is
the **`Mu` type object** while an undeclared `.^auth`/`.^api` are the **empty string**,
and a declared `:api<2>` reads back as the `Str` `"2"` rather than an `Int`. So the
work was purely additive.

## The fix

The declarative adverbs lower to a `__MUTSU_SET_META__` call that writes
`type_metadata[type][ver|auth|api]`, and the readers consult exactly that map. The new
`set_ver`/`set_auth`/`set_api` arm in `src/runtime/methods_classhow_dispatch.rs`
therefore writes the same slot, running the value through the same
`version_from_value` coercion the `:ver(...)` adverb path uses — so the declarative and
programmatic spellings cannot drift apart, and `Versioned.^ver.^name` is `Version`
either way. The three names were added to `Interpreter::is_classhow_method` so HOW
dispatch routes them.

They are deliberately callable after `.^compose`: Rakudo imposes no post-composition
lock on metadata, which is what makes the documented `BEGIN`-block idiom work at all.

Pinned by `t/metamodel-introspection.t`, which asserts the defaults, the declarative
form, and the `.^set_*` round trip side by side.
