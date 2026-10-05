# Collection string searches report Rakudo's suggestions

`List` and `Array` calls to `contains`, `index` and `rindex` now emit Rakudo's
"did you mean" worries and resume with the same stringified-search result.
`Map.contains` and `Map.index` use Map-owned method rows; `Hash` inherits those
rows and receives a warning naming its concrete type.
