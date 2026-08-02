# A `for` loop binds its own topic even when `$_` has a local slot

A `for` block binds `$_` as its own implicit parameter, so it shadows any
enclosing topic. mutsu got that right — except when the enclosing frame held
`$_` in a **local slot**, which is what a routine's `$_` *parameter* produces:

```raku
sub f($_) {
    for 1, 2, 3 { say $_ }      # raku: 1 2 3    mutsu: 99 99 99
}
f(99);
```

The loop bound its item into `env["_"]` only. But when `$_` occupies a local
slot the compiler emits `GetLocal(slot)` for every read of `$_` in the frame,
including inside the loop body, and the slot still held the argument. The topic
of every iteration was therefore the routine's, not the loop's.

`exec_given_op` had already solved exactly this for `given`/`with` (it mirrors
the topic into the slot on entry and restores it on exit), which is why `given`
and `map` were unaffected and only `for` diverged.

The three `for` implementations — the general body loop, the integer-range fast
path, and the two lazy/gather paths — now mirror each item into the topic slot
alongside the `env` write, and restore the entry value on every exit path
(normal, `last`/`next`, error), the same set of exits `restore_loop_topic`
already covered for the env half. The slot is compiler-baked as
`ForLoopSpec::topic_local` rather than resolved by a name scan at loop entry, so
a loop in a frame without a `_` local (the overwhelmingly common case) pays
nothing. It is deliberately distinct from `param_local`, which is the *named*
loop parameter's slot: a `for @a -> $x { }` does not rebind `$_`, and folding
the two would have flipped `writes_back_topic` and broken `$_ = X for @a`.

Found while running the upstream Cro suite: `Cro::HTTP2::FrameSerializer` writes
its frame header through

```raku
method !form-header(Cro::HTTP2::Frame $_) {
    ...
    for 16, 8...0 { $buf[$i] = ($num +> $_) +& 0xFF; $i++; }
}
```

— a `$_` parameter with a `for` loop over the shift widths. Every shift used the
frame object's slot instead of the loop's, so each length and stream-identifier
byte came out as `$num +> 0`: a DATA frame of one byte serialized as
`1,1,1,0,0,1,1,1,1,97` instead of `0,0,1,0,0,0,0,0,1,97`.

Pin: `t/for-topic-shadows-topic-param.t` (also passes under `raku`).
