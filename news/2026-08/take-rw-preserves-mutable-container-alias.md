# `take-rw` preserves a live mutable container alias through `gather`

Resolved 2026-08-30; moved out of `todo/tickets/` during the 2026-09-01 TRIAGE
regeneration. Found by the doc-diff harness (`Type/Mu.rakudoc:531`).

`take-rw` now hands the consumer a live container for an element subscript, a
plain scalar variable, and a `for` loop topic -- including through a lazily
resumed gather:

```raku
my @a = 1,2,3;
for (gather { take-rw @a[0]; take-rw @a[1] }) { $_ = 7 }
say @a;   # [7 7 3] -- raku and mutsu agree

my @b = 1...3;
sub f(@list){ gather for @list { take-rw $_ } };
for f(@b) { $_++ };
say @b;   # [2 3 4]

my $x = 1; for (gather { take-rw $x }) { $_ = 42 }; say $x;   # 42
```

## What fixed it

- `GetScalarContainer` promotes a named scalar at a `take-rw` site and pushes
  its live cell instead of a decontainerized value.
- ADR-0045 supplies a real element cell for the implicit `for` topic.
- A suspended gather retains both its source tag and promoted item cells in
  `ForLoopResumeState::List`, so the source-entry check compares the same cell
  after a consumer has mutated an earlier item, rather than rejecting a stale
  pre-promotion value snapshot.

Affected: `src/vm/vm_for_loop_lazy.rs` (per-item topic read-only decision),
`src/vm/vm_for_loop_body.rs` (`restore_topic_readonly`), `src/compiler/stmt.rs`
(`Stmt::Take` with `is_rw`). Pin: `t/take-rw-shared-cell.t`.

## Two findings that survive this closure

1. `for @a.Seq { $_++ }` still answers `[1 2 3]` in mutsu (raku `[2 3 4]`): a
   `Seq` over an Array does not carry its element containers. Filed as
   `todo/tickets/array-seq-view-does-not-carry-element-containers.md` -- it is
   the same ADR-0045 gap seen from another angle.
2. mutsu's `=:=` is not a reliable identity oracle here: it answers `True` for
   two equal `Int`s in distinct containers (`my $x=1; my $z=1; my @g := (gather
   { take-rw $x }).List; say @g[0] =:= $z` -> mutsu `True`, raku `False`).
   Assert identity by *mutating through the alias and reading the source*, not
   with `=:=`. (Recorded in the new ticket as well.)
