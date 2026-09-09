# ADR-0083: A collected `for` retains lvalue containers past the loop

- Status: Accepted (implemented)
- Date: 2026-09-09
- Supersedes: [ADR-0082](0082-a-collecting-for-gathers-containers-not-snapshots.md)'s
  rejection of collecting a real `ContainerRef`
- Addresses: GitHub issue [#7734](https://github.com/tokuhirom/mutsu/issues/7734)
- Related: [ADR-0040](0040-array-hash-elements-are-itemized-at-the-store.md)
  (ordinary `@` stores decontainerize their input elements),
  [ADR-0059](0059-is-rw-routines-return-a-container.md) (an lvalue result is a
  container), and [ADR-0067](0067-a-routine-hands-back-the-container-it-was-given.md)
  (container identity must survive a return)

## Context

ADR-0082 fixed the in-loop value-collecting `for` shapes by tagging a tail
container and reading its name once after the loop. That was deliberately a
deferred re-read rather than a real cell, so the list itself lost the lvalue
identity at the loop boundary:

```raku
my $g = 1;
my $s = do for 1..2 { $g };
$g = 5;
say $s;                         # (5 5) in raku, (1 1) before this ADR
```

The same divergence occurs for a `:=` binding. The nearby `@` shape is the
important control case:

```raku
my $g = 1;
my @v = do for 1..2 { $g };
$g = 5;
say @v;                         # [1 1] in both implementations
```

The list returned by `do for` must therefore carry the container, while the
later real-Array store must copy its value. The repository already has both
chokepoints: `ContainerRef` is the representation of a shared scalar cell,
and `coerce_to_array` decontainerizes cells when `=` populates an `@` variable.

## Decision

When the VM consumes a `TagContainerRef` signal for a collected iteration, it
resolves the tagged local slot (falling back to the environment) and ensures
that binding is represented by a `ContainerRef`. The cell is installed in the
authoritative local slot and its environment mirror on the first tagged
iteration. Each collected slot then stores the same cell, rather than the
value read from it after the loop.

This applies to the existing tagged lvalue-tail mechanism, including bare
variable tails and assignment-expression tails. It does not broaden the
compiler's tagging rule: loop parameters, `$_`, and body-local `my` bindings
remain per-iteration values; `state` and outer bindings remain shared cells.

The resulting List keeps its cells through scalar assignment and `:=` binding.
When the List is assigned to a real `@` array, `coerce_to_array` decontainerizes
the `ContainerRef` elements before the array's element-store itemization, so
the array snapshot remains `[1 1]`. No special case for this issue is added to
the array consumer.

## Invariants and acceptance

- `my $s = do for 1..2 { $g }` and `my $s := do for 1..2 { $g }` both observe a
  later `$g = 5` as `(5 5)`.
- `my @v = do for 1..2 { $g }` remains `[1 1]` after the later mutation.
- The nine existing rows in `t/for-collect-container-tail.t` remain green,
  including `temp`, `state`, loop-parameter, topic, and body-local cases.
- The five assignment-tail rows in `t/for-collect-assign-container.t` remain
  green; assignment tails continue to collect their lvalue container.

## Consequences

- The collected List now contains first-class `ContainerRef` values for tagged
  lvalue tails. This is the Raku-compatible identity needed after the loop,
  and existing List/Array conversion boundaries determine whether that identity
  is retained or copied.
- The loop no longer performs a deferred end-of-loop value replacement. The
  cell is promoted at collection time, so a mutation between loop completion
  and list consumption is observable only where the result still carries the
  List's containers.
- The compiler's conservative tagging rule remains the boundary for which
  names are eligible; subscript tails remain governed by the separate element
  container and store decisions in ADR-0036 and ADR-0040.

## Alternative rejected

Keep the ADR-0082 deferred name re-read and record the post-loop divergence as
an accepted limitation. That keeps the collected List cheap but contradicts
Raku's lvalue identity exactly at the point where the result escapes the loop.
The existing `ContainerRef` representation and Array store boundary make the
identity-preserving implementation bounded, so the limitation is no longer
justified.
