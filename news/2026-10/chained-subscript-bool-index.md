# A chained element store takes a Bool subscript

`@range[$which][not $upper] = $mid` silently wrote nothing. The chained
element store carries its keys as strings, so a `Bool` subscript became
`"False"`, which names no array element. A non-integer real (`@a[0][1.7]`)
failed the same way. A positional subscript is now converted to its `Int`
first, as a single subscript always was. Geo::Basic's geohash encoder bisects
its ranges exactly this way; all five of its test files pass now.
