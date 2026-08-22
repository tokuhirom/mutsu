# `Range.int-bounds` method is not implemented

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Range.rakudoc:266`).

## Root cause

`Range` has an `int-bounds(\min, \max)` method: it binds integer bounds into the two
`rw` arguments and returns a `Bool` indicating whether the Range has determinable integer
bounds (used as an `if` condition). mutsu does not implement this method at all.

```raku
if (3..5).int-bounds( my $min, my $max) {
    say "$min, $max"; # OUTPUT: «3, 5␤»
}
```

- `raku`: `3, 5`
- `mutsu` (`target/debug/mutsu`): `No such method 'int-bounds' for invocant of type 'Range'`

## Minimal repro

```raku
if (3..5).int-bounds( my $min, my $max) {
    say "$min, $max";
}
```

## Affected files (starting point)

Range methods — likely `builtins/methods_narg.rs` (needs an rw-arg-writing method, similar
in shape to other Range methods that already exist). Needs to compute the Range's integer
min/max (respecting `excludes-min`/`excludes-max`), write them into the two caller-supplied
containers, and return a `Bool` for whether bounds could be determined.
