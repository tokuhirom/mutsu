# A typed array's lone initializer is refused as an element

A typed array's constraint is its element type, so `my Int @a = "x"` makes `"x"`
the array's one element. rakudo refuses it as such:

```
Type check failed for an element of @a; expected Int but got Str ("x")
```

mutsu checked the lone value as though it were a scalar and reported
`Type check failed in assignment to @a`. Its element refusals also dropped
rakudo's hint when a `:D` element is handed its own type object:
`(perhaps Nil was assigned to a :D which had no default?)`. That hint was
missing from `@a[0] = Int` and from a `:D` hash element as well. Both now
match rakudo.
