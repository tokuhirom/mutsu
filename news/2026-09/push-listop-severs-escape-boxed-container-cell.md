# `push()`/`.push()` on a captured typed array stopped severing its container cell

`push(@a, ...)` (both the sub-call and method forms) on an `@`/`%` local that
ADR-0039's escape analysis had boxed into a shared `ContainerRef` cell (a
captured-and-mutated container an escaping closure cannot vouch for — e.g.
`my Str:D @elements` captured by a block passed as a `.map`/`.grep`
argument) silently detached the cell instead of writing through it.

`append`/`unshift`/`prepend`/`pop`/`shift`/`splice` all resolve their target
through `env_root_descended_mut`, which descends through a `ContainerRef`
cell to mutate its held `Array` in place. `push` alone went through a
different helper, `push_to_shared_var`, whose non-shared-array fallback
checked `self.env.get(key)` directly — which never matches `ValueView::Array`
once the slot holds a cell. That fell through to a "rebuild a detached array
and overwrite `env[key]`" path, replacing the cell with a plain array in the
*current* frame only. Every other holder of the cell — in particular an
eager `.map`/`.grep` loop's own saved-env snapshot, restored once the loop
exits — still saw the stale, pre-push cell contents, so the pushed elements
vanished the moment the loop finished:

```raku
sub f($l) {
    my Str:D @elements;
    $l.map({ push(@elements, "x") });
    @elements.join(', ');
}
say f((1, 2, 3));   # raku: x, x, x   mutsu was: (empty)
```

An untyped `my @elements` never took the escape-boxed cell path (ADR-0039
only cells a captured-and-mutated `@`/`%` the creating frame cannot vouch
for), so it was unaffected — which is why the bug looked typed-array-specific
even though the root cause is generic to any escape-boxed container.

Fixed by routing `push_to_shared_var`'s non-shared fallback through
`env_root_descended_mut`, matching every sibling array mutator. This was the
sole remaining blocker for `Config::TOML::Dumper` rendering non-empty arrays
(#7539): `multi sub to-toml(List:D $l --> Str:D) { my Str:D @elements;
$l.map({ push(@elements, .&to-toml) }); ... }` always produced `[  ]`.

Closes #8503.

Pin: `t/collections/array/push-listop-container-cell-writeback.t`.
