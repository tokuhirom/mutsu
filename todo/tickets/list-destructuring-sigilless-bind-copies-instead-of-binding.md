# A list-destructuring sigilless bind copies instead of binding

```
raku  -e 'my ($x,$y) = 1,2; my (\a,\b) := ($x,$y); a = 10; say $x'   # 10
mutsu -e 'my ($x,$y) = 1,2; my (\a,\b) := ($x,$y); a = 10; say $x'
# Cannot assign to an immutable value
```

The **single-variable** form already works in both (`my \a := $x; a = 10` sets
`$x` to 10), and so does hand-unrolling the list form
(`my \c := $p; my \d := $q`). Only the parenthesised list form is broken.

## Root cause (measured with `--dump-ast`)

The list form desugars to

```
my @__destructure_tmp__ = [$x, $y].list;
VarDecl { name: "a", expr: Index { target: ArrayVar("__destructure_tmp__"), index: 0 } }
```

The temp array holds *copies* of `$x`/`$y`, so nothing downstream can reach the
original containers — no amount of element containerization in that temp could
make `a = 10` write to `$x`.

**The fix is in the desugar**: emit N single binds, each to its own RHS lvalue —
which is exactly the form that already works — instead of routing through a
copying temp array.

## Provenance

Filed by ADR-0040 slice 5 (2026-09-02). ADR-0040 §1.7 records this as a claim
that the originating finding
(`news/2026-09/element-itemization-lost-in-scalar-binding.md`) **misfiled** as an
element-itemization symptom: it is a desugar bug, not a container-representation
one, and the ADR deliberately did not cover it. §1.7 also notes the failure mode
changed over time — it used to no-op silently, then died with
`Cannot assign to a readonly variable`, and today says `Cannot assign to an
immutable value`. Re-verified 2026-09-02 against `raku` v2026.07.

## Re-verified 2026-09-02: the prerequisite is gone, the desugar is now the whole job

The failure text is unchanged (`Cannot assign to an immutable value`), but the
blocker underneath it has been removed. `news/2026-09/bind-alias-is-a-container-not-a-name.md`
made a sigilless bind alias **any** lvalue, including an element:

```raku
my @a = 1, 2; my \x := @a[0]; x = 9; say @a;   # [9 2] — was an error
```

That is precisely the shape this ticket's desugar needs, so the "emit N single
binds" advice in the section above can be replaced by something smaller and more
general: keep the staging temp, and make each sigilless target a genuine BIND of
its element rather than a value declaration.

`parse_positional_destructuring` (`src/parser/stmt/decl/destructure.rs:493-640`)
currently emits, per target,

```
VarDecl { name, expr: Index{ ArrayVar("__destructure_tmp__"), i } }
MarkSigillessReadonly(name)
MarkReadonly(name, Immutable)
```

which is a value declaration plus two readonly marks. The single-variable form
that works emits `SyntheticBlock([MarkBind, VarDecl{..}, MarkSigilless(name)])`
(`my_decl_helpers.rs::build_sigilless_bind_stmt`). Emitting the latter shape for
a sigilless target in `is_binding` mode is the change.

**One prerequisite remains** and it is the real one: the staging temp has to
hold the sources' element CONTAINERS. It is built as
`ArrayLiteral([...]).list` and is deliberately excluded from ADR-0040's
element itemization (`Interpreter::is_destructure_staging_temp`), because every
non-binding target reads a value out of it. In binding mode that exclusion is
wrong. Check whether `MakeArray`'s existing `capture_var_cell_inner` path
(ADR-0032 §1.1 lists `my $l = ($a, $b)` as a container-capturing site) already
gives the elements source-aliasing cells when the exclusion is lifted for
`is_binding`.

### Measured divergence table (2026-09-02, debug build vs raku v2026.06)

| # | program | mutsu | raku |
|---|---|---|---|
| D1 | `my ($x,$y)=1,2; my (\a,\b):=($x,$y); a=10; say "$x $y"` | dies, immutable | `10 2` |
| D2 | `... my (\a,\b):=($x,$y); $x=7; say a` | `1` | `7` |
| D3 | `... my ($a,$b):=($x,$y); $x=7; say $a` | `1` | `1` (a `$` target is a read-only COPY, not an alias — do not "fix" this) |
| D4 | `... my ($a,$b):=($x,$y); $a=10` | `Cannot assign to an immutable value` | `Cannot assign to a readonly variable or a value` (message only) |
| D5 | `... my (\a,$b):=($x,$y); a=10; say $x` | dies | `10` (mixed targets) |
| D8 | `my (\a,\b)=(5,6); a=10` | `Cannot modify an immutable Int (5)` | `Cannot assign to a readonly variable or a value` (message only) |
| D9 | `my @z=1,2; my (\a,\b):=@z; a=10; say @z` | dies | `[10 2]` (the RHS need not be a literal list) |
| D10 | `... my (\a,\b):=($x,$y); my $f={a=99}; $f(); say $x` | dies | `99` |
| D11 | three targets, write the third | dies | `1 2 30` |
| D6/D7 | reading `a`/`b` after either spelling | correct | correct |

D9 is the row that rules out "special-case a parenthesised list of variables":
the source can be any list whose elements are containers.
