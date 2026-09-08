# `.raku` of a 1-element array drops the trailing comma when the element is an `is Array` subclass

Split off 2026-09-07 from
`todo/tickets/array-subclass-assignment-in-expression-position.md`
(`news/2026-09/array-subclass-assignment-in-value-position.md`). That fix made
`my @h = $c` store one element as rakudo does; this is the only row of its
matrix still differing, and it is purely about rendering.

## Repro

```raku
class SA is Array { }
my $c = SA.new(3, 2, 1, 4);
my @h = $c;
say @h.elems;      # both: 1
say @h[0].^name;   # both: SA
say @h.raku;
# raku:  [[3, 2, 1, 4],]
# mutsu: [[3, 2, 1, 4]]
```

## Narrowed — it is the ELEMENT's type, not the arity

A 1-element array renders its trailing comma correctly in mutsu whenever the
element is an ordinary container:

| Program | raku | mutsu |
|---|---|---|
| `[[1, 2],].raku` | `[[1, 2],]` | `[[1, 2],]` — correct |
| `my $p = [1,2]; my @q = $p; @q.raku` | `[[1, 2],]` | `[[1, 2],]` — correct |
| **`@h.raku` above (element is an `SA`)** | `[[3, 2, 1, 4],]` | **`[[3, 2, 1, 4]]`** |

So the arity-1 trailing-comma rule is implemented; an `is Array`/`is List`
subclass instance as the element takes a different rendering path that does not
apply it. (`.elems`, `.^name` and the element's own `.raku` all agree, so this
is the outer array's rendering only.)

## A second `.raku` row, same family

A scalar holding such an instance loses rakudo's `$` marker:

```raku
class SA is Array { }
my $c = SA.new(1, 2);
say $c.raku;
# raku:  $[1, 2]
# mutsu: [1, 2]
```

Everything else about that receiver is right — `$c.elems` is 2, `$c.join("-")`
is `1-2`, `for $c` gists `[1 2]`, `$c.push(3)` works — and
`my @h = $c` now stores one element, so this too is rendering only. Both rows
are the holder's `$`/arity marker not reaching an instance-backed container.

## Where to look

The `Array`/`List` `.raku` renderer and its one-element special case — the
branch chosen when an element is an `Instance` carrying
`__mutsu_array_storage` rather than a plain `Array`/`List` value.

## Check when fixing

The repro; the two control rows above, which must not change; the same array
under `.gist` (`[[3 2 1 4]]` in both today — verify against rakudo before
touching it); a 2-element array holding two subclass instances, which must NOT
gain a trailing comma; and `t/array-subclass-value-position-assign.t`, which
pins the assignment half.
