# Atomics on an `int8` / `int16` / `int32` target are refused like MoarVM does

`my int32 $x = 1; $x⚛++` printed 2. Rakudo's integer-atomic candidate takes the container and MoarVM
then refuses it: "Cannot atomic load from an integer lexical not of the machine's native size". The
same holds for **every** atomic on a narrow native integer, the lenient ones (`atomic-fetch`,
`atomic-assign`, `cas`, `⚛$x`, `$x ⚛= v`) included, and the message depends on where the container
lives: an element of `my int8 @a` says "Can only do integer atomic operation on native integer array
element of atomic size", an attribute says "Can only do an atomic integer operation on an atomicint
attribute" ([#12008](https://github.com/tokuhirom/mutsu/issues/12008)).

`native_types::is_atomic_int_target_type` is now the machine-size family (`int`, `atomicint`, `int64`,
`long`, ...) and `is_narrow_atomic_int_type` is `int8`/`int16`/`int32`/`bool`; the runtime verdict gained a
`Narrow` arm that raises the three messages. The strict forms needed nothing else (a declared narrow
type is no longer "proven native", so they get the guard). The lenient forms get a narrow-only guard,
`__mutsu_atomic_narrow_target`, which refuses a target only when it is positively known to be narrow,
so a plain scalar stays legal for them and `my atomicint $n; ⚛$n` compiles without any guard.

Along the way, `@a[0] ⚛= v` turned out to be a plain `IndexAssign` (neither atomic nor guarded); it
now lowers to `atomic-assign(@a[0], v)` in both the statement and the expression parser.

Test: `t/concurrency/thread-lock/atomic-narrow-int-refused.t`. The decision is recorded as ADR-11834 §5.
Closes [#12008](https://github.com/tokuhirom/mutsu/issues/12008).
