# `take-rw $_` doesn't preserve a mutable container alias through `gather` (subscript form fixed)

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Mu.rakudoc:531`).

## Status (2026-08-30): resolved

`take-rw` now preserves a live mutable container alias for element subscripts,
plain scalar variables, and a `for` loop topic through a lazily resumed gather.

```raku
my @a = 1,2,3;
for (gather { take-rw @a[0]; take-rw @a[1] }) { $_ = 7 }
say @a;   # [7 7 3] -- raku and mutsu agree
```

The two former regressions now agree with `raku`:

```raku
my @a = 1...3;
sub f(@list){ gather for @list { take-rw $_ } };
for f(@a) { $_++ };
say @a;   # [2 3 4]

my $x = 1; for (gather { take-rw $x }) { $_ = 42 }; say $x;
          # 42
```

## What fixed it

- `GetScalarContainer` promotes a named scalar at a `take-rw` site and pushes
  its live cell instead of a decontainerized value.
- ADR-0045 supplies a real element cell for the implicit `for` topic.
- A suspended gather now retains both its source tag and promoted item cells in
  `ForLoopResumeState::List`. The source-entry check therefore compares the
  same cell after a consumer has mutated an earlier item, rather than rejecting
  its stale pre-promotion value snapshot.

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
