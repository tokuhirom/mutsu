# A shaped typed array's rows carry their element type, so their holes read as holes

```raku
my Int @a[2;2]; @a[0;0] = 1;
say @a[0;1]:exists;   # raku: False   mutsu was: True
```

`ArrayData::hole_at` decides whether an unwritten slot of a *typed* array is a
gap by comparing the stored type object's name against the array's own
`value_type`. A multidimensional shaped array's rows are arrays in their own
right, and only the top-level array was ever tagged — so every untouched `Int`
cell of a row reported as existing.

The 1D typed form (`my Int @a[3]`) and the untyped 2D form (`my @a[2;2]`) were
both already correct, which is what made the row the odd one out: the reader was
right, the write-side `initialized` tracking was right, and the third input
`hole_at` needs was simply never populated below the outer dimension.

## The fix

`tag_container_metadata` — the chokepoint every declared-constraint assignment
and `SetVarType` tagging already flows through — now recurses into a shaped
array's shaped rows and gives each one the element type.

Deliberately restricted to **shaped** rows. A shaped array's rows are themselves
shaped (`make_shaped_array_seeded` builds them that way), so the recursion cannot
wander into an ordinary nested array that merely happens to sit in a typed
array's element — there the element type describes the element, not the element's
own elements.

Two O(1) guards in front of the walk, both load-bearing. `tag_container_metadata`
runs on the declared-constraint assignment chokepoint — *once per store into a
typed container* — so anything it does per call has to be constant:

- a **native** array's cells hold real zeros, never a type object, so `hole_at`
  never consults `value_type` for one and the walk is pure cost;
- the tagging is all-or-nothing, so if the first shaped row already carries the
  type, the whole array was tagged on an earlier pass.

The first draft had neither, and CI found it: `my int @mat[10001; 10001]` walked
its 10001 rows on every one of its element stores, and
`roast/integration/deep-recursion-initing-native-array.t` went from 8 seconds to
a timeout. Worth remembering — this chokepoint looks like a declaration-time
hook and is not one.

The ticket offered two routes: thread the declared type down through
`make_shaped_array_seeded`'s recursion, or weaken `hole_at`'s `Package` check to
match any non-`Any` type object. Neither was needed. The rows' *cells* already
hold the right `Int` markers by the time the declaration is coerced
(`@a.raku` was already `Array[Int].new(:shape(2, 2), [1, Int], [Int, Int])`); only
the rows' `value_type` was missing, and the tagging chokepoint is where it
belongs.

## The ticket said there was no raku oracle. There is.

It recorded this as "internal self-consistency evidence, not a raku-comparable
regression", because raku dies on `@a[0]` for a shaped array
(`Partially dimensioned views of shaped arrays not yet implemented. Sorry.`).
That is true of the `@a[0].WHAT` probe — but the `:exists` probe beside it in the
same script is what made raku die, not the probe under test. Drop the
`@a[0].WHAT` line and raku answers every row:

| probe | raku | mutsu before | mutsu now |
|---|---|---|---|
| `my Int @a[2;2]; @a[0;1]:exists` | False | True | False |
| `my Str @e[2;2]; @e[0;1]:exists` | False | True | False |
| `my Int @c[2;2;2]; @c[0;0;1]:exists` | False | True | False |
| `my @d[2;2]; @d[0;1]:exists` | False | False | False |
| `my Int @s[3]; @s[1]:exists` | False | False | False |

So this was a genuine raku divergence all along, and it is now verifiable rather
than merely self-consistent. `t/shaped-typed-array-row-hole-tracking.t` pins 17
rows against raku, including `:kv`, `:p`, `:delete`, filling a hole afterwards,
`Array[T].new(:shape(...))`, a fully-initialised shaped array, and the native
element type (`my int @n[2;2]`), whose slots have no type object at all and so
correctly *do* all exist.

**Worth carrying forward as a method:** a "raku has no oracle here" claim is
worth re-testing by splitting the probe. Raku refused the *script*, and the
refusal was attributed to the wrong line in it.
