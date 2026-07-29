# A `use`-loaded sub returns its `with`-block value instead of the tail variable

In a sub loaded from a module via `use`, a statement-position `with` block's
value replaces the value of the bare-variable tail that follows it:

```raku
# TAIL5.rakumod
unit module TAIL5;
sub y2(\ptr) is export {
    my $b = 42;
    with ptr { 2.so; }
    $b;
}
```

```raku
use TAIL5;
say y2(5);      # raku: 42    mutsu: True
```

Three properties make it precise:

- **Only via `use`.** The same file executed as a mainline script (module decl
  and all) answers 42. `--dump-ast` of the module is correct (`body = [VarDecl,
  If{then: [Given …]}, Expr(Var "b")]`), and gdb shows the call executing
  through the VM (`exec_given_op`), not the tree-walk — so the divergence is in
  how the module-load / import path compiles or re-registers the sub, not in
  parsing or in the Given op per se.
- **Only `with`** (which lowers to `If { cond: .defined, then: [Given …] }`).
  A plain `if 1 { 2.so; }` or a bare `given 5 { 2.so; }` before the tail
  variable is fine.
- A sigilless parameter is involved in the original hit but `with $sigiled`
  reproduces too (unverified — re-check when picking this up).

Real-world impact: `NativeHelpers::Blob`'s `blob-from-pointer` has exactly this
shape (`with ptr { …; memcpy($b, ptr, n); } $b;`), so it returns the `memcpy`
result (a `Pointer`) instead of the filled `Buf` — which is what keeps
DBIish's `t/36-pg-blob.rakutest` from reading bytea values back (5 of its 17
subtests). Fixing this unblocks that file and likely `36-pg-array` /
`38-pg-errors` retesting.

Found 2026-07-29 while bringing the DBIish upstream Pg test suite to parity
(see `docs/batteries/dbiish-upstream-bugs.md` for the DBIish-side bugs found in
the same sweep, and `todo/tickets/dbiish-pg-upstream-suite-parity.md` for the
remaining suite gaps).
