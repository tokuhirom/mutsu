# `postcircumfix:<[ ]>` / `<{ }>` called by name now honor adverbs

Raku's subscript operators are ordinary CORE routines, so `postcircumfix:<[
]>(@a, 1, :exists)` and `&postcircumfix:<[ ]>` (the delegation idiom a
module like `Array::Rounded` uses) are supposed to work exactly like the
`@a[1]:exists` syntax. `builtin_postcircumfix_subscript`
(`src/runtime/builtins_postcircumfix.rs`) instead branched purely on
`args.len()`: a named adverb arrives materialized as one more positional
`Value::Pair`, so `postcircumfix:<[ ]>(@a, 0, :nonesuch)` was silently
misread as the 3-arg assignment form and stored the adverb Pair itself into
`@a[0]`.

Named (`Value::Pair`) arguments are now split off from the positionals
before a form is chosen. `:exists`/`:delete` route to the same
`EXISTS-POS`/`EXISTS-KEY`/`DELETE-POS`/`DELETE-KEY` methods the assignment
form already used for `ASSIGN-POS`/`ASSIGN-KEY`, and `:k`/`:v`/`:kv`/`:p`
are computed from a plain read. Any other adverb shape -- an unrecognized
name, or more than one adverb at once -- has no matching CORE candidate and
now raises `X::Multi::NoMatch` instead of corrupting the container.

This also lets `builtins_operators_fallback.rs` drop the exclusion that
kept operator-category names (`infix:<...>`, `postcircumfix:<...>`, ...) out
of the generic user-multi-to-CORE fallback: it existed only to keep
`@a[0]:nonesuch` from reaching this same bug when a user `multi sub
postcircumfix:<[ ]>` is in scope but none of its candidates match.

Pinned by `t/collections/subscript/postcircumfix-subscript-call-adverbs.t`.
