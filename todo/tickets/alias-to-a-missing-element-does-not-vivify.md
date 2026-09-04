# A sigilless alias to a not-yet-existing element refuses the write instead of vivifying

```
raku  -e 'my @a; my \p := @a[5]; p = 9; say @a.raku'   # [Any, Any, Any, Any, Any, 9]
mutsu -e '...same...'                                  # Cannot modify an immutable Package ((Any))

raku  -e 'my %h; my \p := %h<x>; p = 9; say %h.raku'   # {:x(9)}
mutsu -e '...same...'                                  # Cannot modify an immutable Package ((Any))
```

Measured 2026-09-04 against `raku` v2026.06.

An EXISTING element aliases and writes through correctly
(`my @a = 1, 2; my \p := @a[0]; p = 9` gives `[9 2]`, pinned by
`t/bind-alias-is-a-container.t`), so this is specifically the
not-yet-vivified case: the bind reads the element as the `Any` type object
rather than as the deferred vivification token that a later write could
resolve, and `MarkSigillessBindSource` then (correctly, given what it is
handed) settles the term as immutable.

## Where to look

mutsu already has the deferred-token machinery — `try_deferred_token_index_assign`
(`src/vm/vm_var_assign_element.rs`) resolves one for the `$`-sigil spelling
(`my $x := %h<g>; $x[0] = 'x'`) — so the question is why the sigilless bind path
does not receive a token for a missing element. Compare what
`IndexAutovivifyLazyTerminal` leaves on the stack for a present versus a missing
element; the present case yields a promoted `ContainerRef` cell and the missing
one yields the bare type object.

The `$`-sigil twin diverges the same way and should be fixed with it:

```
raku  -e 'my @a; my $p := @a[5]; $p = 9; say @a.raku'  # [Any, Any, Any, Any, Any, 9]
mutsu -e '...same...'                                  # Cannot modify an immutable value
```

## Provenance

Split out while landing `news/2026-09/list-literal-element-container.md`
(2026-09-04). That change made a LIST LITERAL carry an array/hash element's
container, so `my @a = 1, 2; my (\p, \q) := (@a[0], @a[1]); p = 9` writes
`[9 2]`; the missing-element spelling
(`my @a; my (\p) := (@a[5],); p = 9`) still fails, and it fails identically
without any list in the way — which is what localises it here rather than in the
list compile.
