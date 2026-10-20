# A native array's `.append` adds an itemized Iterable's elements

`my int @o; @o.append: $x`, where `$x = [1, 2]`, used to apply the `Array`
one-argument rule. That rule keeps an itemized argument whole, so mutsu pushed
the Array as one element and died with a type check. Rakudo's native
`array.append(\value)` candidate adds the elements of any single Iterable,
itemized or not, so mutsu now does the same. An `Array`, and a native array's
`prepend`, still keep an itemized argument whole.

PDF's LZW filter appends `@table[$code]` elements to a `my uint8 @out`, so it
could not decode a stream before this change (#14232, #12907).
