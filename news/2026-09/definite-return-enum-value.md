# A `-->` return spec naming an enum value now returns that value

`sub f($x --> B) { }` (where `enum E <A B C>`) is the idiomatic way to write
a typed constant-returning routine — Raku treats a `-->` whose spec is a
*constant value* rather than a type as an unconditional return of that
value, regardless of the body. mutsu already got this right for literal
spellings (`--> 42`, `--> "lit"`, `--> Nil`) but not for an enum value:
`--> B` was read as a type constraint named `B`, the (empty) body's `Nil`
passed that "constraint" unconditionally (since it never resolved to an
actual type), and the routine answered `Nil` instead of `E::B`.

## Root cause

Whether a `-->` spec is a constant value or a type constraint is decided by
`is_definite_return_spec`, which exists in two copies that must agree:
`Compiler::is_definite_return_spec` (a static function with no interpreter
access, driving whether the body's last expression is sunk) and
`Interpreter::is_definite_return_spec` (driving the actual return-value
substitution). Neither treated an uppercase enum-value spelling as
anything but "an unrecognized uppercase name, therefore a type constraint".

Fixed both to recognize a known enum value:

- The compiler consults the parser's own thread-local enum-value registry
  (`register_user_enum_value` / `is_user_declared_enum_value`), newly
  re-exported crate-wide for this — the only enum-membership fact available
  to a function with no interpreter to ask.
- The interpreter consults the actual runtime enum-key namespace
  (`Interpreter::enum_bare_value`).

A third bug surfaced once the discriminators agreed: the value-construction
path (`evaluate_definite_return_value`) fell back to `self.eval_eval_string(s)`
for a bare name it did not special-case, but `EVAL`-compiled code does not
resolve an enum's bare key from the enclosing lexical scope the way ordinary
bareword resolution does (`EVAL('B')` itself fails with "Undeclared name"
even outside this feature, a pre-existing and separate gap). Reading the
value directly out of the enum-key bare-name namespace, alongside the other
already-special-cased constants (`Nil`, `True`, `pi`, ...), sidesteps it.

The enum's own TYPE name (`--> E`, or another enum entirely like `-->
CardTypes2`) is unaffected: it is a separately-registered user type and
still type-checks the return value.

## Regression

`t/types/enum-subset/definite-return.t` gained three assertions: a `-->`
spec naming an enum value returns it, the enum's own type name in `-->`
still constrains rather than substitutes, and a real type mismatch there
still raises `X::TypeCheck::Return`.

Fixes #8022 (found via `Business::CreditCard`'s
`multi sub cardtype($, *% --> NotACreditCard) { }` dirty-input fallback
candidate, which answered `Nil` for every non-card input instead of
`NotACreditCard`).
