# `has $.n = Nil` via `bless`, and a `$`-held Seq stays one element

Two gaps blocked Format::Lisp. Its tests compare `is-deeply` trees of
directive objects and `join` the formatted pieces of a `~C` directive.

## `has $.n = Nil` through `bless`

A `Nil` store into a Scalar resets it to its default: `Any` for an untyped
attribute, the type object for a typed one, or the `is default(...)` value.
`.new` did this for a literal `= Nil`, but `self.bless` kept a literal `Nil`, and
neither path did it for an initializer that evaluates to `Nil`. Now both do.
`has $.d is default(5) = Nil` also gives 5 through `.new`; it used to give
`Any`.

## A `$`-held Seq

A Seq stored in a `$` container is itemized (its view is `ItemSeq`), but the
shared flattener (`flat_val`) spread it like a bare Seq. So
`join("-", $t)` gave `1-2` instead of `1 2`, and `(1, $t).flat.elems` was 3. It
now stays a single element, as an itemized List already did. Format::Lisp's
`join('', map { ...; $text }, @directives)` depends on this.

All 32 Format::Lisp test files now pass. The two that differed now produce the
same output as rakudo.
