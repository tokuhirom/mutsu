# Dynamically-parameterized native arrays now wrap out-of-range values on push

A statically-declared native typed array has always wrapped out-of-range
values on `.push`, matching C unsigned/wrapping semantics:

```raku
my uint8 @a;
@a.push(-1);
say @a[0];   # 255
```

But an array built via a *dynamic* (runtime-computed) `array[T]`
parameterization and bound with `:=` — `array[$cond ?? T1 !! T2].new` —
stored the pushed value unwrapped:

```raku
my $is-signed = False;
my $a := array[$is-signed ?? int8 !! uint8].new;
$a.push(-1);
say $a[0];   # was -1 in mutsu; raku (and now mutsu) says 255
```

The root cause was a dual-store staleness, not a metadata-propagation gap as
originally suspected. `array[EXPR].new` correctly tags the constructed array
with its resolved element type (`array[uint8]`, readable via `.of`)
regardless of whether the type parameter was a compile-time literal or a
runtime expression. The bug was in how `.push`/`.unshift`/`.append`/`.prepend`
read that metadata back: `$a.push(...)`, compiled to the `CallMethodMut`
opcode (because the receiver `$a` is a scalar-sigil variable, not an
`@`-sigil one — a `:=`-bound scalar never takes the specialized `ArrayPush`
fast path), calls `wrap_native_int_items()` to pre-wrap the pushed values
*before* popping the receiver off the VM stack. That helper looked the
receiver up by name via `self.env().get(target_name)`.

For a plain-lexical scalar, though, the interpreter's dual local-slot/env
store treats the local slot as authoritative and leaves the env mirror at
its `my`-declaration seed until some later sync point (an I/O op like `say`,
a frame boundary, ...) republishes it. `$a := array[...].new` writes only the
local slot, so at push time `env().get("a")` returned a stale, untagged copy
of the array — even though `$a[0]` and `$a.of` (which read the authoritative
slot) already reported the correct `uint8` element type. This explained the
ticket's odd secondary symptom: inserting *any* unrelated statement (even
`say "x"`) before the `.push` made the bug disappear, because `say` happens
to trigger an env-from-locals resync as a side effect.

The fix mirrors the pattern already used by the sibling element-assignment
and `:delete` chokepoints (`seed_env_from_scalar_slot`): before
`wrap_native_int_items` reads the receiver by name, `exec_call_method_mut_op_impl`
(`src/vm/vm_call_method_mut_ops.rs`) now seeds the env mirror from the
receiver's local slot when it is a scalar. This makes the native-int wrap
chokepoint see the same authoritative, metadata-tagged value that `.of` and
indexed reads already did.

Added `t/dynamic-array-parameterization-push-wrap.t`, covering both signed
and unsigned dynamic parameterizations at 8-bit and 16-bit widths, a
multi-value push, and the `unshift`/`append` mutators that share the same
`wrap_native_int_items` chokepoint.
