# A list literal holds an array/hash element's container, not a copy of it

`todo/tickets/list-literal-does-not-capture-element-containers.md`: a `List`
literal already held the CONTAINER of a scalar-variable element, so an alias of
that list element reached the variable —

```raku
my $x = 1; my $l := ($x, 6); my \a := $l[0]; a = 10; say $x;   # 10, both
```

— but not of an array or hash ELEMENT, so the list stored a dereferenced copy
and the alias refused the write:

```
raku  -e 'my @a = 1, 2; my (\p, \q) := (@a[0], @a[1]); p = 9; say @a'   # [9 2]
mutsu -e '...same...'   # Cannot modify an immutable Int (1)
```

## Cause and fix

`Expr::ArrayLiteral`'s element loop tags an element with `WrapVarRef` only when
`scalar_container_alias_name` finds a source NAME (`Expr::Var` and friends), and
`exec_make_array_op` captures a container only for a tagged element. An
`Expr::Index` has no name, so it fell through and the list stored the value.

The fix does not extend the name-based tag; it compiles such an element the way
a `:=` bind to a subscript already does (`scalar_bind_autovivify` +
`bind_terminal`), so the element REFERENCE itself reaches `MakeArray`. A
`ContainerRef` is a scalar item, so it never flattens, and every reader derefs
it — which is why nothing a list literal renders or copies changes.

That the reference is a **deferred vivification token** is what makes it safe:
`my @a; my $l := (@a[5],)` leaves `@a` empty, exactly as rakudo does, instead of
growing it to six elements at list-construction time.

## Measured blast radius

The ticket asked for "its own measurement pass", since this changes what every
parenthesised list holds. Every one of these agrees with rakudo before and
after — the container is only ever observable through an alias:

| | rakudo |
|---|---|
| `(@a[0], @a[1]).raku` | `(1, 2)` |
| `(@a[0], @a[1]).WHAT` | `(List)` |
| `my @b = (@a[0], @a[1]); @b[0] = 9` | copies; `@a` unchanged |
| `sub f(*@x) {…}; f(@a[0], @a[1])` | values |
| `[(@a[0], @a[1])]` | `[1, 2]` (decontainerized) |
| `my @a; (@a[5],)` | `@a` stays `[]` |

`make test` (3646 files) and a full local `make roast` (1436 files, 218962
tests) are green.

## Left open

Writing through an alias to a **not-yet-existing** element still refuses
(`my @a; my (\p) := (@a[5],); p = 9`). That is not a list-literal gap — the
direct `my \p := @a[5]; p = 9` and the `$`-sigil `my $p := @a[5]` fail
identically — so it is filed on its own as
`todo/tickets/alias-to-a-missing-element-does-not-vivify.md`.

## Coverage

`t/list-literal-element-container.t` — 15 assertions, all dual-oracled against
rakudo: destructured and after-the-fact aliases of array, hash and
computed-index elements; the scalar-variable control; the render/copy/slurpy/
bracket-array rows above; and the no-vivification-at-construction rule.
