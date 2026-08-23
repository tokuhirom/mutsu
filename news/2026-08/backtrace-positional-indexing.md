# `$!.backtrace[N]` now returns the Backtrace::Frame instead of Nil

Positional indexing into a `Backtrace` object always answered `Nil`/`Any`,
regardless of the index:

```raku
sub zipi { { { die "Something bad happened" }() }() };
try { zipi; }
say $!.backtrace[0];      # was: Nil
say $!.backtrace[*-1];    # was: Nil
```

`Backtrace` was already Positional everywhere *except* the subscript. List
context unwrapped its `frames` attribute (`runtime/utils/list.rs`), and
`.list`/`.elems`/`.map`/`.grep`/`.head`/`.tail` all worked off it, so
`$bt.list[0]` answered the right frame while `$bt[0]` did not.

## Root cause

`Backtrace` is an `Instance` whose frames live in a `frames` attribute, and no
arm of the subscript dispatch in `vm/vm_var_index_ops.rs` knew about it. An
`Int` index therefore fell through to the generic `(Instance, Int)` arm, which
tries the class's `AT-POS`/`AT-KEY` methods — `Backtrace` had neither — and
returned `Nil`, which the typed-container default then turned into `Any`. The
whatever-star form `[*-1]` was worse: its `(Instance, Sub)` arm derives the
element count from a `Buf`'s bytes or a `Match`'s `list` attribute only, so a
`Backtrace` measured as zero-length and `*-1` evaluated to `-1`, again `Nil`.

## Fix

Rather than re-deriving each index shape for `Backtrace`, the subscript is now
delegated to the stored `frames` List, the way the `__baggy_data__` arm already
delegates to its inner Bag/Set: push the frame list back onto the stack and
re-enter the index op. Every index shape a List supports therefore works, and
works identically to a List — single index, `[*-1]`, `[0,1]` slices, `[^2]` and
`[0 .. *-1]` ranges, and `[*]`. Because Rakudo keeps a `Backtrace`'s frames in a
`List` rather than an `Array`, an out-of-range index correctly reads back as
`Nil` (not the `Any` an Array hands out) — mutsu's List subscript already
matched raku here, so the delegation inherits it for free.

The explicit `AT-POS` spelling (`$bt.AT-POS(0)`, previously an
`X::Method::NotFound`) is implemented alongside it in
`builtins/methods_narg/dispatch_1arg.rs`, with a matching row in the native
method table so introspection reports it.

Verified against real `raku`: the new `t/backtrace-positional-index.t` passes
unmodified under both `raku` and `mutsu` (21 assertions covering every index
shape above, out-of-range, `.AT-POS`, and the associative subscript staying
undefined).

## Deliberately not changed

mutsu's captured backtrace has fewer frames than Rakudo's (4 vs 7 for the repro
above) because Rakudo includes internal setting frames such as
`SETTING::src/core.c/Exception.rakumod`'s `Exception.throw` that mutsu's frame
capture has no equivalent for. That is a frame-model difference, not an
indexing bug, and it is tracked separately in
`todo/tickets/backtrace-frame-indexing-returns-nil.md`.
