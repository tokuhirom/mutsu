# `.head`/`.tail`/`.first` as an lvalue now mutate the container, not a name

`@array.tail` yields a writable reference to the array's last element, so
`@array.tail ~= 'x'` mutates that element in place. This worked for a plain
lexical `my @a` but silently no-opped when the receiver was a class-attribute
array (`@!numbers.tail ~= 'x'`), which is how `Language/grammars.rakudoc`'s
`Digifier`/`Devanagari` actions example accumulates digits — the accumulation
never landed, and a later `.tail` on the (still empty) array eventually threw
`X::Assignment::RO` as a downstream symptom.

## Root cause

The rw write-back for `.head`/`.tail`/`.first` in
`assign_method_lvalue_with_values` (`src/runtime/methods_mut_method_lvalue.rs`)
built a **brand-new array** with the element replaced and then re-bound it under
the target's *name*:

```rust
let replacement = Value::array_with_kind(Gc::new(ArrayData::new(updated)), kind);
if let Some(var_name) = target_var {
    self.env.insert_through(var_name.to_string(), replacement);
}
```

A name-keyed rebind only ever reaches a plain lexical. For `@!numbers` the
target name is `"@!n"`, which has no env entry of record — reads go through the
instance's attribute map — so the rebuilt array was written somewhere nothing
reads it and the mutation vanished. A closure-captured array had the same
problem for the same reason.

## Fix

A new primitive, `Value::array_set_in_place` (`src/value/value_methods_b.rs`) —
the existing-slot counterpart of `array_push_in_place` — writes element `idx`
through the shared `Gc<ArrayData>`, routing through
`Value::assign_element_slot` so a slot already promoted to a `ContainerRef` cell
is written *through* rather than replaced (a `:=` binding to that element sees
the write too).

`write_array_slot_lvalue` (a new helper next to the three call sites) tries the
in-place write first and only falls back to the old rebuild-and-rebind when
`target` is not an Array. Mutating the one canonical container reaches every
holder by construction — lexical, public attribute, private attribute, closure
capture — instead of requiring each holder to be reachable by name.

## Verified

`t/attribute-container-identity.t` (green under both `raku` and mutsu) covers
`.tail ~=` on a private attribute array, on a public attribute array, and on a
plain lexical; `.head =` on a lexical; and `.tail ~=` observed through a closure
that captured the array. Each of those also asserts that the array **object
identity** is unchanged by the write (`===` before/after), so a future rebuild
regression fails loudly rather than silently working by value.
