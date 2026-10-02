# A type-check message names every offending value by its `.raku`

Rakudo words a failed store as `expected R but got <Type> (<.raku>)` for *every*
offending value, the `.raku` cut to 20 characters plus `...` when longer than 23.
mutsu printed the tail for a string, an integer, a type object or an object only:

```
$ raku  -e 'role R {}; class C { has R $.r }; try C.new(:r((1, 2, 3))); say $!.message'
Type check failed in assignment to $!r; expected R but got List ((1, 2, 3))

$ mutsu -e 'role R {}; class C { has R $.r }; try C.new(:r((1, 2, 3))); say $!.message'
Type check failed in assignment to $!r; expected R but got List
```

`List`, `Array`, `Hash`, `Pair`, `Range`, `Set`/`Bag`/`Mix`, `Seq`, `Complex`,
`Version` and `Regex` all fell into the same silence, and the scalar kinds that
*did* print a tail were wrong in a different way: `value_short_repr` hand-rolled
its own spelling, so `Bool::True` read `True`, `1e0` read `1`, `3.14` read
`157/50`, `1/3` read `1/3` rather than `<1/3>`, an enum value lost its
`Color::` prefix and a string was not escaped.

`value_short_repr` now renders every value through `raku_value`, the one pure
`.raku` renderer, so the message agrees with the method by construction. A
`Sub`, an object, or a collection holding one still needs method dispatch, so
`Interpreter::type_check_got_repr` renders those (a collection through the same
leaf dispatch `.raku` itself uses). The value is named out of its `$` container,
as rakudo does: a `for` variable holding `(1, 2, 3)` reads `List ((1, 2, 3))`,
not `List ($(1, 2, 3))`.

Rendering the scalars through the shared renderer exposed that the direct
`Rat.raku` method and the `Rat` arm of `raku_value` rounded a terminating decimal
through an `f64` (`123456789.123456789` read `123456789.12345679`). Both now
spell the exact decimal.

Still different from raku, and not covered here: the `.raku` of a built-in object
type such as `Blob`/`Buf`/`IO::Path`, and of a lazy `Seq`
([#10677](https://github.com/tokuhirom/mutsu/issues/10677)).
