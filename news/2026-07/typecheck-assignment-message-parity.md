# `X::TypeCheck::Assignment` now matches Rakudo's wording, and names the attribute

Rakudo's assignment type-check message is

```
Type check failed in assignment to $x; expected Int but got Str ("s")
```

mutsu produced a shorter form on several paths — `expected Int, got Str`, with
no short representation of the offending value — and, on the accessor path,
echoed the syntax used at the assignment site (`$.n`) where Rakudo always names
the attribute (`$!n`). Both were recorded as `todo/tickets/` findings; this
closes them together, since they are the same message on the same paths.

## What was wrong

Three separate divergences, all in the same message:

1. **The short template.** `runtime/utils/errors.rs::type_check_assignment_error`
   already built the correct wording, but the typed-lexical path
   (`value/error_typed.rs::typecheck_assignment`) formatted its own
   `expected X, got Y`. So `my Int $x = "s"` and `Foo.new(n => "s")` produced the
   short form while an array element or a return value produced the long one.
2. **An untyped exception on the `.new` attribute path.**
   `methods_object_attr_constraints.rs` raised a bare
   `RuntimeError::new(format!(…))` there, so `Foo.new(n => "s")` came out as
   `X::AdHoc` — Rakudo raises `X::TypeCheck::Assignment`, with `.expected` and
   `.got` populated.
3. **`$.n` instead of `$!n`.** Assigning through an `is rw` accessor
   (`$.n = $v` inside a method, or `Foo.new.n = "s"` from outside) reported the
   source-level name. Rakudo reports the attribute whichever syntax wrote it.

## Fix

`typecheck_assignment` now builds the same `expected X but got Y (repr)` text as
the shared formatter, reusing `value_short_repr` so the two cannot drift. The
`.new` attribute constraint check goes through
`type_check_assignment_typed_error`, so it raises a real
`X::TypeCheck::Assignment` with the right message and attributes. And
`format_var_name_for_error` normalizes a `.`-twigil accessor name to the `!`
twigil (`$.n` → `$!n`, likewise for `@`/`%`/`&`), which fixes every assignment
path at once rather than each remembering on its own.

All eight shapes in the reduced comparison (typed `my` of a builtin and of a user
class, the `.new` attribute path, the accessor path from inside and outside a
method, an array element, a hash value, and a return value) are now
byte-identical to `raku`.

`t/typecheck-assignment-got.t` asserted the old short form; its expectation was
rewritten to Rakudo's. That test's regex was not valid raku either (an unquoted
`,` inside `/…/`, which raku `SORRY`s on), so it now parses under both.

Pin: `t/typecheck-assignment-message-parity.t` (10 tests, identical output under
`raku`), covering the wording, the exception type and its `.expected`/`.got`, the
`$!`-naming from both accessor directions, and the element/return-value messages
that must stay unchanged.

## Left open

The `:D`/`:U` attribute-default check still raises
`X::TypeCheck::Attribute::Default` with the old short wording where Rakudo raises
an `X::TypeCheck::Assignment` reading `expected Int:D but got Int (Int) (perhaps
Nil was assigned to a :D which had no default?)`. That is an exception-type
change rather than a message tweak, so it is left for its own slice.
