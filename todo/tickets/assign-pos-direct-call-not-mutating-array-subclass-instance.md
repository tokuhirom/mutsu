# A direct `.ASSIGN-POS(...)` call on an `is Array` subclass instance does not mutate the backing storage

Found while implementing the write-side fix for
`todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md` (a user
3-arg `multi sub postcircumfix:<[ ]>` candidate whose body delegates to
`SELF.ASSIGN-POS($index, $value)`, the idiomatic way such a candidate stores
into the native backing array).

## Repro

```raku
class Foo is Array {}
my $f = Foo.new(1, 2, 3);
$f.ASSIGN-POS(1, 99);
say $f[1];        # raku: 99   mutsu: 2 (unchanged)
say $f.AT-POS(1);  # raku: 99   mutsu: 2 (unchanged)
```

Confirmed against real `raku` (prints `99` twice). mutsu prints `2` twice —
the call silently succeeds (no error) but never mutates the instance's
`__mutsu_array_storage`.

Contrast: `@a.ASSIGN-POS(0, 99)` on a **plain** (non-Instance) `@`-sigil array
variable works correctly in mutsu already, and subscript-syntax assignment
(`@p[0] = 99` where `@p is PCPlain = 1,2,3` and `PCPlain is Array`) ALSO
already works — so this is narrowly the *direct method call* path for an
Array-subclass **Instance** receiver, not a general ASSIGN-POS gap.

## Where it breaks

`src/vm/vm_call_method_mut_ops.rs`, the `CallMethodMut` Array-subclass
delegation block (~line 2260 onward): `is_array_method` includes
`"ASSIGN-POS"`, but `Self::native_array_storage_mut` (line 2876) has no
`"ASSIGN-POS"` arm, so it returns `None` and falls through to the documented
slow-path fallback (comment at ~line 2395: "`.splice`/`ASSIGN-POS`/… mutate,
so those keep the fallback — they need the first-class element-cell
write-back the interpreter owns"):

```rust
let result = loan_env!(self, call_method_mut_with_values(
    "__mutsu_array_tmp", storage.clone(), &method, args,
))...
if let Some(updated_storage) = self.env().get("__mutsu_array_tmp").cloned() {
    storage = updated_storage;
}
self.env_mut().remove("__mutsu_array_tmp");
self.write_back_array_storage_instance(&target_name, &inst_class, &attributes, inst_id, storage);
```

This binds `storage.clone()` under a synthetic env var `"__mutsu_array_tmp"`
and calls `ASSIGN-POS` on it by name, expecting the same in-place mutation a
direct `@a.ASSIGN-POS(...)` on a real named array variable gets (confirmed
above that this DOES work for a real named `@`-sigil variable). But reading
`"__mutsu_array_tmp"` back afterward yields the unmutated original, so
whatever `call_method_mut_with_values` does for `"ASSIGN-POS"` either isn't
writing through the synthetic binding the same way the normal `CallMethodMut`
opcode dispatch does for a source-level named variable, or the env write
happens on a different (COW-copied) `Arc` than the one re-read.

Not investigated further — needs a `rust-gdb` breakpoint comparing the two
call paths (`@a.ASSIGN-POS(...)` opcode dispatch vs. this
`call_method_mut_with_values` direct-call site) to see where the mutated Arc
diverges from what `self.env().get("__mutsu_array_tmp")` reads back.

## Why this matters

Any user postcircumfix/operator-overload candidate (or plain user code) that
calls `.ASSIGN-POS`/`.ASSIGN-KEY` directly on an `is Array`/`is Hash`
*subclass instance* (not a plain container) silently no-ops instead of
mutating — a genuine, general correctness gap, not specific to any one
module. `Array::Rounded`-style dists that delegate assignment to the native
operator via `old-same SELF, $index` (once
`todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md` item 2,
the `&postcircumfix:<[ ]>` callable term, is fixed) will hit this if their
`old-same` delegate itself calls `ASSIGN-POS`.
