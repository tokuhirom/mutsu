# A slice subscript argument is not an `is rw` writeback target

`compile_call_arg_with_escape` queued an `is rw` snapshot/writeback pair for
every `Expr::Index` call argument. It made no distinction between an argument
that addresses one element (`f(@a[1])`, which really can bind to an `is rw`
parameter and really does need a writeback) and one that addresses a *slice*
(`f(@a[1..*])`, `f(@a[0,1])`, `f(%h<a b>)`), which yields a list of values and
has no storage location at all.

Rakudo rejects the slice bind outright:

```raku
sub f($x is rw) { $x = 9 }
my @a = 1, 2, 3;
f(@a[1..*]);       # X::Parameter::RW: expects a writable container (variable)
say @a;            # (1 2 3) -- untouched
```

mutsu bound it and then wrote the callee's scalar back over the slice, so `@a`
came out as `(1 9)` — the tail was eaten. A hash slice was worse:
`h(%g<a b>)` set *both* keys to the callee's value.

## The second failure: a writeback that fires long after its guards were valid

The same queue also produced a writeback with nothing to write. It is guarded
by a `===` comparison against a snapshot and then an `eqv` against the target's
current value, but both guards are computed over compile-time-*fixed global*
temp names (`__mutsu_index_rw_arg_N` / `__mutsu_index_rw_orig_N`). A recursive
descent that re-enters the same call site clobbers them, and when the call's
result is a deferred `.map` Seq that is reified after the producing frame is
gone, neither guard matches any more. The writeback then assigns into whatever
the argument expression names at that later moment.

`roast/integration/99problems-21-to-30.t`'s P26 is exactly that shape:

```raku
sub group(@sizes, @elems) {
    return $[] if @sizes == 0;
    map -> $e {
        map -> $g { $[ [|@$e], |@$g ] },
             group(@sizes[1..*], grep { not $_ === any(@$e) }, @elems)
    }, [combination(@sizes[0], @elems)]
}
```

`group` recurses on `@sizes[1..*]` and returns a nested lazy `map`. Under the
vendored upstream `Test` module — whose `is` reifies the Seq later than mutsu's
native TAP provider does — the file died with `Cannot modify an immutable List
((2 1))` and reported "planned 15, ran 12". The bug was in the argument-passing
path, not in either `Test` provider; the module only changed *when* the Seq was
pulled.

## The fix

A statically-visible slice index — a range or sequence operator (`1..*`,
`0..^2`, `1...*`), a bare `*`/`**`, or a literal index list (`0,1`, `<a b>`) —
now compiles as the plain value read it is: no snapshot temps, no
`WrapVarRef`, no writeback. A subscript whose index only turns out to be a
Range at runtime (`@a[$r]`) keeps the existing treatment.

Dropping the `WrapVarRef` is what lets the binder raise `X::Parameter::RW` on
its own, so all four shapes now match rakudo exactly, error included.

ADR-0059 slice 3 retires these temps for *every* subscript argument; this
removes the class that could never have been right.

Pinned by `t/rw-slice-arg-is-not-a-container.t` (9 subtests, identical output
under `raku`).
