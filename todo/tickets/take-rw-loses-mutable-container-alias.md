# `take-rw $_` doesn't preserve a mutable container alias through `gather` (subscript form fixed)

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Mu.rakudoc:531`).

## Status (2026-08-26): partially fixed; the remaining half needs ADR-0045

`take-rw` of an **element subscript** now works end to end and matches `raku`:

```raku
my @a = 1,2,3;
for (gather { take-rw @a[0]; take-rw @a[1] }) { $_ = 7 }
say @a;   # [7 7 3] -- raku and mutsu agree
```

What still diverges is `take-rw` of a **plain scalar or of the loop topic**:

```raku
my @a = 1...3;
sub f(@list){ gather for @list { take-rw $_ } };
for f(@a) { $_++ };
say @a;   # raku: [2 3 4]   mutsu: "Cannot resolve caller postfix:<++>(_)"

my $x = 1; for (gather { take-rw $x }) { $_ = 42 }; say $x;
          # raku: 42        mutsu: "Cannot assign to an immutable value"
```

## What was fixed, and why the rest is blocked

Two things had to be true for a `take-rw`'d value to stay writable:

1. **The gathered element must actually BE a live container.** `take-rw`
   compiles its operand with `:=`-bind semantics (`scalar_bind_autovivify` +
   `bind_terminal`), and that flag pair only affects `Expr::Index` /
   `Expr::MultiDimIndex`, which emit `IndexAutovivifyLazyTerminal` /
   `MultiDimIndexBindRef` and promote the element to a shared `ContainerRef`
   cell. A plain `Expr::Var` compiles to `GetLocal`/`GetGlobal` and pushes a
   value copy — verified with `rust-gdb` breaking on `Interpreter::take_value`,
   where `val.is_container_ref()` is `false` for `take-rw $x`. (mutsu's own
   `my $y := $x` does not produce a cell either; scalar `:=` is implemented as
   a **name alias** via the `__scalar_bind` VarDecl trait, which a value pushed
   onto a gather buffer cannot carry.)

2. **The consuming `for` must not force the topic read-only.**
   `exec_for_loop_lazy_gather_from` marked `$_` `ReadonlyKind::Immutable`
   unconditionally, before the loop, on the (usually correct) grounds that "the
   topic aliases the item directly, with no container of its own". That decision
   is now made **per item**: an item that IS a `ContainerRef` leaves `$_`
   writable, and the write goes through the cell to the original element. The
   pre-loop save/restore of the topic's read-only kind was reworked accordingly
   (`Interpreter::restore_topic_readonly`).

Point 2 shipped and is what makes the subscript form work. Point 1 is the
blocker for the two remaining spellings:

- `take-rw $x` (plain lexical) would need the lexical **boxed into a shared
  cell** at the take site. mutsu has the machinery (`Value::into_container_ref`
  plus the locals/env/shared-store rewrite that `atomic_scalar_cell` performs
  for `⚛`/`cas`), but wiring it up needs a new opcode for "promote this named
  scalar to a cell and push it", and the promotion is observable to every other
  alias of that name.
- `take-rw $_` (the ticket's own repro) needs more than that: `$_` inside
  `for @list` is bound to the element's **value**, and `@list`'s elements are
  written back by name at the end of each iteration. Boxing `$_` would capture a
  cell that is disconnected from `@a[i]`, and the writeback would have already
  run by the time the gathered sequence is consumed. Making this work is exactly
  [ADR-0045](../../docs/adr/0045-for-loop-parameters-bind-the-element-container.md)
  ("a `for` loop parameter binds the element *container*; the per-iteration
  writeback is retired"), which is `Proposed`, design-complete, implementation
  not started. Special-casing `take-rw` of the topic against the loop's
  `container_binding` + index would be exactly the kind of ad-hoc route
  CLAUDE.md rules out.

## Measured `raku` semantics (v2026.06), for whoever picks this up

| expression | raku |
| --- | --- |
| `my @a=1,2,3; for (gather for @a { take-rw $_ }) { $_ = $_*10 }; say @a` | `[10 20 30]` |
| `my @a=1,2,3; my @g = gather for @a { take-rw $_ }; @g[0]=99; say @a` | `[1 2 3]` (array assignment copies) |
| `my $x=1; for (gather { take-rw $x }) { $_ = 42 }; say $x` | `42` |
| `my $s := gather for @a { take-rw $_ }; $s.List[0] =:= @a[0]` | `True` |
| `for (gather for @a { take $_ }) { $_++ }` | dies: "requires mutable arguments" |
| `for @a.Seq { $_++ }` | `[2 3 4]` (a `Seq` over an Array carries its element containers) |
| `for @a.List { $_++ }` | dies: `List` decontainerizes |

Note the last two: mutsu answers `[1 2 3]` for **both**, because `@a.Seq` does
not carry element containers either (`my $s = @a.Seq; $s.List[0] =:= @a[0]` is
`False` in mutsu, `True` in raku). That is the same ADR-0045 gap seen from
another angle, and is a useful second acceptance test for it.

Also note that mutsu's `=:=` is not a reliable oracle here: it answers `True`
for two equal `Int`s in distinct containers (`my $x=1; my $z=1; my @g :=
(gather { take-rw $x }).List; say @g[0] =:= $z` → mutsu `True`, raku `False`).
Assert identity by *mutating through the alias and reading the source*, not with
`=:=`.

## Affected files

- `src/vm/vm_for_loop_lazy.rs` — per-item topic read-only decision (fixed).
- `src/vm/vm_for_loop_body.rs` — `restore_topic_readonly` helper (added).
- `src/compiler/stmt.rs` (`Stmt::Take` with `is_rw`) — where a plain-`Var`
  operand would have to emit a cell-promoting op.
- `docs/adr/0045-for-loop-parameters-bind-the-element-container.md` — the
  blocker for the `take-rw $_` spelling.
