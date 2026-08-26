# `.splice` type-checks the values it actually stores, and says so in `raku`'s words

The ticket started as "`.splice`'s inserted replacement values are never
type-checked at all"; that general gap was closed earlier. Two things survived,
and both are now fixed.

## Root cause

`.splice`'s type check ran on the **raw** `args[2..]`, not on the values
`do_splice` would go on to store. Both remaining divergences fell out of that
one ordering mistake.

`flatten_splice_replacement_args` is where two decisions about the stored values
are made: splice's one-arg rule (a *lone* `Positional` contributes its elements,
several contribute themselves) and ADR-0049's `Nil` → plain `Any` decay
(splice, unlike push/append/unshift/prepend, does **not** use the container's
`is default(...)` for a spliced-in `Nil`). Checking beforehand meant:

- A `Nil` replacement was skipped by an explicit `!v.is_nil()` guard — it looked
  like a "reset to default" marker — and the `Any` it decayed into was never
  re-checked. `my Int @a = 1,2,3; @a.splice(1,0,Nil)` silently produced
  `Array[Int].new(1, Any, 2, 3)`; `raku` throws.
- The checker flattened **every** `Array` argument, not just a lone one, so
  `@a.splice(1,0,@b,@c)` passed its check on `@b`/`@c`'s elements while
  `do_splice` correctly inserted the two `Array`s themselves. `raku` throws
  "expected Int but got Array (Array)".

Separately, the exception carried the generic element-store message — "Type
check failed for an element of @a; expected Int but got Str" — where `raku`'s
`X::TypeCheck::Splice` says "Type check failed in splice; expected Int but got
Str (Str)", naming the operation and repeating the type object's `.raku` in
parentheses. Same exception class, but code matching on `.message` saw a
different string.

## Fix

Both type-check blocks in the `"splice" =>` arm
(`src/runtime/methods_mut_dispatch.rs` — the name-based one and the
container-metadata one for a subscripted element) now iterate the result of
`flatten_splice_replacement_args(args[2..])`, i.e. the exact `Vec<Value>`
`do_splice` inserts, and the `is_nil()` skip is gone with it. The messages were
rewritten to `raku`'s wording, and the reported `got` type now goes through
`utils::got_type_name`, so a type object names *itself* (`Any`) rather than the
`Package` its runtime representation is.

Checking the post-flatten values is also what makes the ordering
self-maintaining: any future change to splice's flattening or `Nil` handling is
automatically reflected in what gets validated.

`t/buf-and-list-mutators.t` pins a wrong-typed non-`Nil` insert, a `Nil` insert
into a typed array (with the exact message), a `Nil` insert into an *untyped*
array (still allowed), a splice with no replacement values, a lone good `Array`
(flattens and passes), and several `Array`s (do not flatten, and are rejected as
`Array`s).
