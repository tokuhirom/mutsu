# Attribute-default type checks now raise rakudo's exception types and wording

`todo/tickets/attribute-default-typecheck-exception-type.md` reported that a
`:D`/`:U` attribute-default type check raised the wrong exception type. Measuring
against raku turned up a clean rule the ticket had not seen, and mutsu was wrong
on both sides of it.

## The rule

Rakudo splits attribute-initializer failures in two, by whether the default is
*defined*:

| default | when | exception |
| --- | --- | --- |
| a **defined** value that can never satisfy the constraint — wrong type, or any defined value under `:U` | at **declaration** | `X::TypeCheck::Attribute::Default` — `Can never assign default value Str ("str") to attribute '$!n', it expects: Int:D` |
| a **type object** (or `Nil`) failing a `:D` | at **construction** | `X::TypeCheck::Assignment` — `Type check failed in assignment to $!n; expected Int:D but got Int (Int)` |

The `(perhaps Nil was assigned to a :D which had no default?)` hint is not
unconditional: it appears exactly when the reported `got` type object *is* the
constraint's own nominal type. `Str:D = Nil` gets it (assigning `Nil` resets the
container to `Str`, so `got` is `Str`); `Str:D = Int` does not (`got` is `Int`).

## What mutsu did

- `has Int $.n = "str"` and `has Int:D $.n = "str"` — **no error at all**. The
  never-satisfiable case was simply not checked.
- `has Int:U $.n = 5` — right exception type, but mutsu's own wording
  (`Type check failed in default value of attribute $!n; expected Int:U, got Int`).
- `has Int:D $.n = Int` — `X::TypeCheck::Attribute::Default` where rakudo raises
  `X::TypeCheck::Assignment`, and `got Package` (mutsu named the internal
  representation of a type object instead of the type).
- `has Str:D $.n = Int` — reported `expected Str`, dropping the smiley.
- `has Str $.n = Nil` — read back as `Nil`; raku gives the type object `Str`,
  exactly as `my Str $x = Nil` already did in mutsu.

## The fix

Five pieces, four of them shared machinery rather than attribute-specific:

- `value_short_repr` gained a type-object arm (`(Int)`), and a new
  `got_type_name` reports a type object as **itself** rather than `Package`.
  Both feed every type-check message (assignment, element, binding), so
  `my Str:D $x = Int` picked up rakudo's exact wording for free.
- `definite_type_check_assignment_error` builds the `:D` failure and appends the
  "perhaps Nil" hint under the rule above (`is_nominal_type_object_of`).
- `attribute_default_never_assign_error` builds the "Can never assign default
  value ..." error.
- `validate_static_attribute_default` (in `registration_class_decl.rs`) rejects a
  defined literal initializer that can never satisfy the constraint when the
  class is declared — reached from both the class-body `HasDecl` walk and the
  runtime `RuntimeHasDecl` path. A *type-object* default is deliberately not
  decidable there and is left to the construction-time smiley check.
- The attribute smiley check reports the constraint with its smiley reattached
  (`attribute_constraint_with_smiley` / `attribute_reported_constraint`), and a
  literal `Nil` initializer now seeds the declared type object
  (`methods_object_default_ctor.rs`), matching what a typed variable already did.

The required-attribute `:D` branch, which had its own copy of the message with
an unconditional hint, now goes through the shared builder too.

Pin: `t/attribute-default-typecheck.t` — 10 assertions, each verified against
raku first.

## Residue, split out

Three `:D`/`:U` message divergences remain on the *variable* declaration path
(a different dispatch): `my Int:D $x = Int` and `my Int:U $x = 5` drop the smiley
from `expected`, and `my Str:D $x = Nil` reports `Nil` instead of the type object
it resets to. Recorded in
`todo/tickets/typed-variable-definite-message-residue.md`.
