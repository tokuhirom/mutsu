# `.tail` used as an lvalue for compound assignment on a private attribute array silently no-ops

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/grammars.rakudoc:289`).

## Root cause hypothesis

`@array.tail` returns a writable (rw) reference to the array's last element, so
`@array.tail ~= 'x'` (or any compound-assignment op) should mutate that element in place.
This works for a plain lexical `my @a` array. It does **not** work when the receiver is a
private class-attribute array (`has @!numbers;`, accessed as `@!numbers.tail`): the
compound assignment silently no-ops instead of mutating the last element, and the array
appears unchanged afterward. When `.tail` is later called on what *should* be a non-empty
array but the accumulated pushes never actually landed (each iteration's mutation was
dropped), an empty array's `.tail` correctly returns `Nil`, which then throws
`X::Assignment::RO: cannot assign through .tail on non-instance` on the next attempted
`~=` — this is *downstream* of the real bug, not a separate one.

## Minimal repro

```raku
class Foo {
    has @!numbers;
    method go() {
        @!numbers.push: '';
        say @!numbers.elems;      # 1 -- correct, push worked
        say @!numbers.raku;       # [""] -- correct
        @!numbers.tail ~= 'x';
        say @!numbers.raku;       # should be ["x"]
    }
}
Foo.new.go;
```

- `raku`: prints `1`, `[""]`, `["x"]`
- `mutsu`: prints `1`, `[""]`, `[""]` — the `.tail ~= 'x'` assignment is a no-op

For comparison, the same operation on a plain lexical array works correctly on both:

```raku
my @a = [1,2,3];
@a.push: 0;
@a.tail ~= 5;
say @a;   # both raku and mutsu: [1 2 3 05]
```

This is how it surfaces in the doc's grammar-actions example
(`Digifier`/`Devanagari` in `grammars.rakudoc`): `method digit ($/) { @!numbers.tail ~=
<...>[$/] }` accumulates digits into the last pushed element of a private attribute array,
and because the mutation never lands, the array stays effectively empty and a later
`.tail` throws `X::Assignment::RO`.

## Affected files (starting point)

- Wherever `.tail` (and likely `.head`/other rw-container-returning accessor methods) is
  implemented as an lvalue target for compound assignment — probably in
  `src/builtins/methods_0arg/` or `src/vm/vm_var_assign_*.rs`. The write-back path likely
  resolves the container cell correctly for a `my @a` local's array storage but not for an
  `Instance` attribute's array storage (`@!name`), which may use a different storage/cell
  representation (`vm_var_assign_computed_attr.rs` handles attribute writeback elsewhere in
  the codebase and may be the right model to follow).
