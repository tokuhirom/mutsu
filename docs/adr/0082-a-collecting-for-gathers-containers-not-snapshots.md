# ADR-0082: A value-collecting `for` gathers containers, not snapshots

- Status: Superseded by ADR-0083
- Date: 2026-09-09
- Related: [ADR-0045](0045-for-loop-parameters-bind-the-element-container.md)
  (a `for` parameter binds the element container),
  [ADR-0059](0059-is-rw-routines-return-a-container.md) and
  [ADR-0067](0067-a-routine-hands-back-the-container-it-was-given.md) (a routine
  hands back a container, not a copy of its value),
  [ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md) (a Pair
  from a subscript carries the element container),
  [ADR-0052](0052-a-when-clause-produces-its-value-on-the-stack.md) (a `when`
  clause's value reaches the collecting loop on the stack)
- Addresses: [#7718](https://github.com/tokuhirom/mutsu/issues/7718). Depends on
  [#7677](https://github.com/tokuhirom/mutsu/issues/7677) (a loop body resolves
  its own `let`/`temp` saves, PR #7722), which landed while this was in flight.
  Findings filed out of scope:
  [#7720](https://github.com/tokuhirom/mutsu/issues/7720) (the same `temp` gap,
  still open for `if`/`unless`/`with` branches);
  [#7721](https://github.com/tokuhirom/mutsu/issues/7721) (withdrawn)

## Context

A Raku block's value is **not** decontainerized. A collecting `for` whose body
ends in `$g` therefore gathers the `Scalar` container `$g` denotes, and every
collected slot reads it when the list is consumed — not when the iteration ran:

```raku
my $g = 1; my @v = do for 1..2 { temp $g = 9; $g };      # raku: [1 1]
my $g = 1; my @v = do for 1..2 { $g = $g + 1; $g };      # raku: [3 3]
my $g = 1; my @v = do for 1..2 { temp $g = 9; $g + 0 };  # raku: [9 9]
```

The third line is the control: `$g + 0` is a value expression, so it opts back
out and the snapshot is right there.

mutsu answered `[9 9]`, `[2 3]` and `[9 9]`. The loop pushed each iteration's
*value*, so the first two were snapshots taken mid-iteration.

Half the mechanism already existed. `compile_expr_assign` emits
`OpCode::TagContainerRef` after an assignment used as an expression, and
`exec_for_loop_body` records the tagged slot in `deferred_container_refs` and
re-reads it once the loop is over — which is why `do for 1..3 { $s += $_ }` was
already `(6 6 6)` (`t/for-collect-assign-container.t`). What had no tag was the
other shape a container reaches tail position in: a **bare variable read**.

The first line has a second bug in it, and it is not this one: `temp` in a loop
body was never restored, so even a correctly collected container still held 9.
That is #7677, fixed by PR #7722 before this change landed. This ADR is only
about what the loop *collects*.

## Decision

### 1. A bare variable read in tail position is tagged as a container

When the body of a collecting `for` ends in `Stmt::Expr(Expr::Var(_))`, the
lowering emits `TagContainerRef` for that name, so the loop collects the
container the same way it already collects a tail assignment's lvalue.

The tag is emitted **after** `compile_scope_restored_body_value`, so outside the
body's own `let`/`temp` save frame (#7677) — `exec_let_block_op` jumps the ip
past everything inside that frame's range, and this tag has to run. Being
outside it is what the container semantics want anyway: the tag records a name
whose value is read once the whole loop is over.

### 2. Only a container that outlives the iteration qualifies

A name is tagged only when it is not rebound per iteration:

| Tail | raku | Reason |
| --- | --- | --- |
| `do for 1..2 { $g }` (outer `my`, `our`) | `(1 1)` | one container for the loop — **tagged** |
| `do for 1..3 { state $s = 0; $s = $s + $_; $s }` | `(6 6 6)` | `state` storage is one cell — **tagged** |
| `do for 1..3 -> $i { $i }` | `(1 2 3)` | the parameter is rebound per iteration |
| `do for 1..3 { $_ }` | `(1 2 3)` | so is the topic |
| `do for 1..3 { my $x = $_ * 2; $x }` | `(2 4 6)` | a body `my` is a fresh container per iteration |
| `do for 1..2 { temp $g = 9; $g + 0 }` | `(9 9)` | not a variable read at all |

`state` is the one declaration inside the body that is still tagged, and it is
the case that proves the rule is about *storage*, not about where the name was
written: mutsu answered `(1 3 6)` before this ADR.

Twigil'd and punctuation names (`$*d`, `$!a`, `$^a`, `$/`) are deliberately left
alone. They do not resolve through the plain lexical lookup the VM's re-read
uses, and nothing here has measured what raku does with them in this position.

### 3. The re-read is slot-first, env-fallback

`deferred_container_refs` now carries the tag's compile-time-baked local slot
alongside the name, and the end-of-loop re-read goes through
`gate_local_slot_at` before falling back to `get_env_with_main_alias`. That is
the §1.5 order of `docs/lexical-scope-slot-campaign.md`: a plain `my $g` keeps
its live value in its slot and its env mirror is suppressed, so the env-only
read this mechanism shipped with saw `Any` for every `my` lexical. It went
unnoticed because the only shape that reached it — a tail assignment — happens
to refresh the env on its way through.

### What was rejected

**Collecting a real `ContainerRef` cell instead of a deferred name.** This is
the container-shaped answer, and it is the one raku actually implements: it
would also fix `my $s = do for 1..2 { $g }; $g = 5; say $s`, which raku prints
as `(5 5)` and mutsu (still) prints as `(1 1)`, because the collected list holds
the container past the loop rather than a value read at its end. It was rejected
for *this* change because it puts `ContainerRef` cells into an ordinary Array
and its blast radius is every consumer of a collected list — a cost these three
repro lines do not justify. The deferred re-read is the mechanism the repository
already chose for the assignment shape; this extends it rather than introducing
a second one beside it.

**Widening the rule to a subscript tail.** `do for 0..2 -> $i { @a[$i] }` is an
element container in raku too, but the collected list is decontainerized at the
store before anything can mutate `@a`, so mutsu's snapshot already agrees
(`[1 2 3]`, and a later `@a[0] = 9` does not change it). There is no measured
divergence to fix, and a tag there would need the element's identity rather than
a name.

## Consequences

- `src/compiler/control_for.rs` owns the tagging, so the statement and
  expression positions get it together — the drift this module was created to
  end (see its header) does not reopen.
- The end-of-loop re-read is now the mechanism's only reader of a variable, and
  it goes through the same slot-first chokepoint as the rest of the VM. Any
  future tag site inherits that.
- `t/for-collect-container-tail.t` pins nine rows and runs green under both
  `mutsu` and `raku`, so the oracle is in the suite rather than in this
  document. The per-iteration `temp` restore its first row depends on is
  #7677's, pinned by `t/loop-body-let-resolution.t`.
- Still divergent, by choice: a collected list does not survive as containers
  past the loop (`my $s = do for 1..2 { $g }; $g = 5; say $s`). Reopen with the
  rejected alternative above if that shape ever matters.
