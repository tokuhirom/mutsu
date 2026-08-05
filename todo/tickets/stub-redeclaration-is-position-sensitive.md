# Stub redeclaration is position-sensitive (works in some files, dies in isolation)

Raku allows redefining a yada-stub routine without `supersede`:

```raku
sub lightning {...}
sub lightning {42}
say lightning();   # raku: 42
```

mutsu dies with `Redeclaration of routine 'lightning'` — at the top level, in
a bare block, and with a statement between the two declarations. Yet the same
shape PASSES inside `t/stub-and-supersede.t`. Bisecting the difference
(2026-08-06, during ADR-0019 C6e-3a) showed the passing behavior depends on
*later* content of the file: adding a second block containing
`use MONKEY-TYPING; sub hail {26}; supersede sub hail {8};` AFTER the
stub/redefine block makes the earlier block's redefinition succeed, and
removing it makes the earlier block die. A trailing pragma block changing the
registration behavior of an earlier, unrelated block means some hoisting or
pragma state is applied file-globally rather than lexically.

Also observed: the `use MONKEY-TYPING`+`supersede` block itself dies with
`Redeclaration of routine 'hail'` when the file's block layout differs from
the original test file (same code, different position → different outcome).

Repro files from the session: the shapes above as one-liners
(`sub lightning {...}; sub lightning {42}; say lightning()`) fail
deterministically; `t/stub-and-supersede.t` passes. Mode-independent —
identical with and without `MUTSU_DROP_LEGACY_BODY=1`.

Affected area: `register_sub_decl_with_metadata`'s hoist/in-sequence
re-registration interplay (`registration_sub.rs` — the
`existing_is_stub`/`allow_lexical_shadow` guards and the hoist pass
ordering). Likely fix direction: make stub-tolerant redefinition depend only
on the existing def's `is_stub` fact, independent of hoist order, and scope
`MONKEY-TYPING` lexically.
