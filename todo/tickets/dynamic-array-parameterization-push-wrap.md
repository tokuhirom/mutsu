# A dynamically-parameterized native `array[T]` does not wrap out-of-range values on `.push`

## Symptom

A statically-declared native typed array wraps out-of-range values on
`.push`, matching C unsigned/wrapping semantics:

```raku
my uint8 @a;
@a.push(-1);
say @a[0];   # 255 (correct, matches raku)
```

But an array built via a *dynamic* (runtime-computed) `array[T]`
parameterization — `array[$cond ?? T1 !! T2].new` — stores the value
unwrapped instead:

```raku
my $is-signed = False;
my $a := array[$is-signed ?? int8 !! uint8].new;
$a.push(-1);
say $a[0];   # mutsu: -1 (wrong); raku: 255
```

This was found as a side effect of fixing
`todo/tickets/cbor-simple-typed-array-and-diagnostic-format-gaps.md` (which
added lowercase `"array"` to `is_non_parametric_type`'s allowlist in
`src/runtime/runtime_class_query.rs`, letting `array[EXPR]` — a computed
type-parameter expression — construct a native array at all instead of
throwing `X::NotParametric`). Once construction worked, the wrap-on-push gap
became visible; it is a **separate, narrower bug**, not the same root cause.

## Root cause (partial)

Native-int wrap-on-store (`Interpreter::wrap_native_int_by_constraint` /
`validate_native_int_assignment` and the array-push counterparts in
`vm_data_push_ops.rs`) is keyed off the **lexical variable's
compile-time-registered type constraint** (`var_type_constraint`), which a
statically-declared `my uint8 @a` sets via `SetVarType` at declaration time.
A `:=`-bound array built from a dynamic `array[EXPR].new` expression never
registers such a constraint against the binding name — the array VALUE
itself carries its element type (readable via `.of`), but the push path
apparently does not consult that value-level metadata as a fallback when no
lexical constraint is registered.

## Why this is out of scope for a quick fix

- Not needed by the CBOR::Simple use case that motivated the original
  ticket: `06-typed-arrays.rakutest` passes in full (36/36) without this —
  its decoder never pushes an out-of-range value onto one of these arrays
  (values are read from validated CBOR-encoded native-endian bytes, already
  in range for their declared width).
- The right general fix likely needs the array push/store paths to fall back
  to the target VALUE's own declared element type (from its `ArrayKind`/
  container metadata) when no lexical `var_type_constraint` is registered,
  rather than adding another special case — worth doing carefully alongside
  a broader look at how `.of`/native-array metadata is threaded through
  `vm_data_push_ops.rs` and the indexed-assignment paths, not as a one-line
  patch.

## Repro

```raku
my $is-signed = False;
my $a := array[$is-signed ?? int8 !! uint8].new;
$a.push(-1);
say $a[0];   # mutsu: -1; raku: 255
```

Also reproduces for signed overflow (`push(300)` onto a dynamically-typed
`int8` array should wrap/error like the static form).
