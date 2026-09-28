# Subscript call arguments bind the element's own container

ADR-0059 Slice 3 is done. A single-dimension subscript passed to a named
routine (`g(@a[0])`, `g(%h<k>)`) used to be compiled as a value read plus a
copy-in/copy-out protocol: the element was snapshotted into two
`__mutsu_index_rw_arg_N` / `_orig_N` call temps, the call's `is rw` parameter
wrote back into the temp by name, and a guarded `===`/`eqv` writeback after the
call re-assigned the element. Those temps, their env-merge exclusions, the
`INDEX_RW_CALL_TEMP` symbol flag and the `GetCallTemp` opcode are gone.

Instead the named call reuses ADR-0067's `IndexArgRef` producer, with a new
`RwArgCallee::Named` gate: when some registered candidate of the callee (or,
failing that, the lexical `&g`) binds that positional to the caller's container
(`is rw`, `is raw`, or a sigilless `\x`), the subscript hands over the
element's location; otherwise it is a plain `Index`, as before. The same
producer now serves a user-defined infix operator's operands
(`@a[1] plus_égal 5`), and an expression-level `($x := @a[0])` binds the
element's cell like the statement form does.

The producer no longer grows an array or creates a hash key just to hand it
over. Past the end it passes the deferred vivification token that `my $r :=
@a[5]` already used, so the first write through the parameter creates the
element and a read creates nothing — the read-safety the ADR had recorded as
Slice 3's blocker.

A *nested* subscript (`g(%h<a><b>)`) cannot be answered by one op — its
missing intermediate level has already been read as `Any` — so the same gate
runs up front (`RwArgCalleeBindsContainer`) and branches to the `return-rw`
operand's container-mode chain, whose missing levels are deferred. Such a
chain now also defers an intermediate array step past the end instead of
growing the array (`my @a; my $x := @a[1][2]` leaves `@a` empty), and the
deferred token of a missing hash key reads as the hash's `is default` value or
value type, as the array half already did.

Removing the temps fixed three bugs they carried, all now matching rakudo:

- `rw(@a[$i++])` wrote `@a[1]` and left `$i` at 3: the writeback re-evaluated
  the index expression.
- `two(|@c, @a[1])` with `sub two($p, $q is rw)` never wrote back.
- `sub raw(\x) { x = 8 }; raw(%h<a>)` died with "Cannot modify an immutable
  Package"; `x.VAR.^name` for such a parameter said `Any` instead of `Scalar`.

Pinned by `t/collections/subscript/index-arg-container-mode.t`.
