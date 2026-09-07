# A subscript argument loses its container: the residue

The headline of this ticket is **CLOSED (2026-09-07)** by ADR-0067's
subscript-ARGUMENT producer, `OpCode::IndexArgRef` — the twin of
`IndexInvocantRef` one position over, replacing the argument's trailing `Index`
and gated at run time on the real callee. See
`news/2026-09/slice-first-and-block-topic-element-containers.md`, pinned by
`t/block-call-binds-topic-raw.t`. These all bind the element now:

```raku
sub g($y is rw) { $y = 9 }
class S { method take($y is rw) { $y = 9 } }
my @a = 1, 2;

g(@a[0]);                             # already worked (CallFunc's temp protocol)
S.new.take(@a[0]);                    # now [9 2]
my $r = &g; $r(@a[0]);                # now [9 2]
&g(@a[0]);                            # now [9 2]
(-> $x is rw { $x = 9 })(@a[0]);      # now [9 2]
my $b = { $_ = 9 }; $b(@a[0]);        # now [9 2] -- the bare-block topic half
```

The ticket's own root-cause reading was right, and so was its prediction that
the fix wants "its own runtime gate on the same callee question" rather than
extending `CallFunc`'s copy-in/copy-out temp protocol to the other three
dispatch shapes.

## What is still open

Three rows, all measured 2026-09-07 against raku v2026.06:

- **`$obj.^lookup('m')($obj, $c.v)`** — raku `9`, mutsu still refuses. A `Method`
  object invoked as a code value takes its invocant as positional argument 0, so
  the accessor is positional 1 counting the invocant; the code-value gate
  (`code_value_binds_container_at`) reads `SubData::param_defs` and skips
  invocant parameters, so it looks for parameter 1 of a signature that has one.
  Fixing it means knowing, from the code value alone, that it is a `Method` whose
  invocant is passed explicitly.
- **`sub g(:$y is rw) { }`** — raku refuses the *declaration* at compile time
  (`Cannot use 'is rw' on optional parameter '$y'`); mutsu accepts it and refuses
  at the call site instead. A parser validation gap, unrelated to containers; the
  same is true for the `method` spelling.
- **An OUT-OF-RANGE subscript argument** (found by the fix's own probe). The
  NAMED callee already vivifies correctly through `CallFunc`'s temp protocol
  (`g(@a[5])` gives `[1 2 (Any) (Any) (Any) 9]` in both), so this is a gap
  specific to the three nameless-callee shapes:

  ```raku
  my @a = 1, 2; sub g($y is rw) { $y = 9 }; my $r = &g; $r(@a[5]);
      # raku:  [1 2 (Any) (Any) (Any) 9]
      # mutsu: dies, "expects a writable container ... '(Any)' (Any)"
  my @a = 1, 2; my $b = { $_ = 9 }; $b(@a[5]);
      # raku:  [1 2 (Any) (Any) (Any) 9]
      # mutsu: [1 2] -- the write is silently dropped
  ```

  `IndexArgRef` deliberately declines past the end: it shares
  `take_subscript_element_cell` with the receiver producer, whose contract is
  "an existing element of a real Array/Hash" (`@a[5].mut` has no element), and
  `Value::array_slot_ref` answers a *terminal* out-of-range index with a deferred
  `HashEntryRef` vivification token rather than a cell — which is right for
  `my $r := @a[5]` but is a path token, not a container the `is rw` binder or the
  topic binder accepts today. So closing this means either teaching those two
  consumers to accept a deferred token, or giving the argument producer its own
  eager-growth gate; either way the two producers stop sharing one rule, which
  wants its own decision rather than a quiet divergence.
