# `.list` of a native array is a `List`

`my uint32 @b = 1, 2; @b.list` answered the array itself
(`array[uint32].new(1, 2)`); Rakudo's native arrays are no `List`, so `.list` is
a `List` of their elements. The `Any.list` and `List.list` rows now share one
implementation for an `Array`, which makes that `List`.
