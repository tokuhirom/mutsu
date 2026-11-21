# An itemized native array keeps its type through `.head(n)`, `.sort` and kin

`my num @a = ...; my $c = @a; $c.head(2)` is an `array[num]` in Rakudo, as `@a.head(2)` is: a
native array's order-preserving methods (`head`, `tail`, `sort`, `reverse`, `unique`, `squish`,
`rotate`) rebuild the receiver's own type. mutsu retyped the answer only for a bare `@` receiver,
not for the same array held in a `$` (an `ItemArray`). So `$c.sort` answered a `Seq`, and for
`$c.head(n)` the method-table lane answered a `List` while the full call path answered the native
array; debug builds panicked on the disagreement (Math::Wavelet's `t/07-cwt.t`) and release
builds took the lane's wrong answer (#18389). The retyping now looks through the item container,
so both paths answer the native array.
