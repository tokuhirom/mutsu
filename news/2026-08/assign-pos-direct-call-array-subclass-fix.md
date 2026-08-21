# Direct `.ASSIGN-POS(...)` method call now mutates `is Array` subclass instances

`class Foo is Array {}; my $f = Foo.new(1, 2, 3); $f.ASSIGN-POS(1, 99);` used
to silently no-op instead of mutating the backing storage — `$f[1]` and
`$f.AT-POS(1)` both still read `2` afterward instead of `99`, even though
subscript-syntax assignment (`$f[1] = 99`) and a direct `.ASSIGN-POS(...)`
call on a plain, non-Instance `@`-sigil array both already worked correctly.

Root cause: in the `CallMethodMut` Array-subclass-instance delegation
fallback (`src/vm/vm_call_method_mut_ops.rs`), methods without a dedicated
native handler (`.ASSIGN-POS`, `.DELETE-POS`, `.BIND-POS`, ...) dispatch
through `call_method_mut_with_values("__mutsu_array_tmp", storage.clone(),
...)`. These methods mutate by scanning `self.env` for a binding whose Array
`Gc` pointer identity matches the receiver
(`overwrite_array_bindings_by_identity`) and writing the update through that
binding — they do not return an updated value through `target_var` directly.
A real named `@a.ASSIGN-POS(...)` call works because `@a` is already present
in `self.env` with the matching pointer at call time. The synthetic
`"__mutsu_array_tmp"` binding, however, was only ever *read back* after the
call, never *seeded* into `self.env` before it — so the identity scan found
nothing to write through, and the mutation silently vanished.

Fixed by seeding `self.env["__mutsu_array_tmp"]` with `storage.clone()`
immediately before the dispatch call, giving the identity-based write-back
scan something to find — mirroring what already happens for real named array
variables. Regression test: `t/array-subclass-assign-pos-direct-call.t`
(covers `.ASSIGN-POS` mutating and being visible via both subscript and
`.AT-POS()` reads, growing the array past its current length, and
`.DELETE-POS`, which shares the same fallback/write-back path).

While investigating, the ticket's Hash-subclass `.ASSIGN-KEY` speculation
("probably a parallel bug") turned out to be a much larger, separate,
pre-existing gap rather than a small parallel fix — `is Hash` subclasses have
no Instance-delegation subsystem at all (no `__mutsu_hash_storage` attribute
convention, no `is_associative_base` helper, missing even `.AT-KEY` and
subscript-read support), plus an independent constructor bug where `Bar.new`
with no named args silently degrades to a bare `Hash` value instead of a
`Bar` instance. That was split off into its own finding:
`todo/deep/hash-subclass-instance-has-no-method-delegation.md`.
