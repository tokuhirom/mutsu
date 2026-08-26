# Three of the four smiley / element-check parity gaps closed

`todo/tickets/native-type-smiley-and-element-check-gaps.md` collected four
parity gaps split out of the `NativeHelpers::Blob` work. Three are now closed;
the fourth turned out to need a representation change and the ticket has been
rewritten around it alone.

## Closed: a `$`-sigil parameterised container checked elements against itself

```raku
my array[uint8] $a .= new(1, 2);
$a[0] = 7;
# raku : [7 2]
# mutsu: Type check failed for an element of $a; expected array[uint8] but got Int (7)
```

The element-store check in `exec_index_assign_expr_named_op_inner` compared the
assigned value against the variable's whole declared type. It already knew that a
container type on a `$` variable describes the *container* — `my Hash $h; $h<k> =
1` stores an Int, not a Hash — and exempted an allow-list of bare container names
for that reason. But the exemption matched on the full constraint string, so
neither `array[uint8]` nor `Array[Int]` was in it.

Both halves of that were wrong for a *parameterised* container: Rakudo checks each
element against the parameter (`$a[0] = "x"` is `X::TypeCheck`, `$a[0] = 7` is
not). The new `scalar_container_element_constraint` reduces a `$`-sigil
constraint to the element type it implies — the parameter for a parameterised
container, nothing for a bare one, and the constraint unchanged for a
non-container — which subsumes the old allow-list rather than extending it. A
multi-parameter spelling (`Hash[Int,Str]`, whose second parameter is the *key*
type) is left unconstrained rather than checked against the parameter list.

`@`/`%` variables are untouched: there the constraint IS the element type, and
`my Array @a; @a[0] = 1` is still a type error.

## Closed: `.^name` kept the `:_` smiley

`(Int:_).^name` reported `Int:_`; Rakudo reports `Int`, because `:_` is the
"either" smiley and constrains nothing (`:D` and `:U` *are* kept by both).
`user_facing_type_name` now normalises it away, which is a display-only site
alongside the ADR-0056 NativeCall qualification — constraint matching reads the
real name through `strip_type_smiley` and is unaffected. `.raku`, `.gist` and
`.WHAT.^name` follow for free.

## Closed: a type object had no `.ACCEPTS`

`Int.ACCEPTS(5)` and `(Int:D).ACCEPTS(5)` both threw `X::Method::NotFound`, even
though `5 ~~ Int:D` worked — the constraint logic existed but was unreachable
through the explicit method spelling. The gap was wider than the ticket claimed:
the *unsmileyed* `Int.ACCEPTS(5)` failed too. `call_method_with_values_inner`
already answered `Mu.ACCEPTS` for plain scalars by delegating to `smart_match`;
it now does the same for a type object, deferring to a user-declared `method
ACCEPTS` on the class when there is one.

The ticket's warning was honoured: `(array:U).ACCEPTS($a)` is still `False`,
which is what real Rakudo answers for the method spelling (rakudo's `True` for
the literal `$a ~~ array:U` is a quirk mutsu deliberately does not copy).

## Pin

`t/native-type-smiley-and-element-checks.t`, 22 assertions, passing identically
under `raku` and mutsu.
