# `.VAR` on a `ContainerRef` is not distinguishable from the aliased value itself

## Symptom

A value that *is* an element container (a `ContainerRef` cell — what
`array_slot_ref`/`hash_slot_ref` hand out, and what ADR-0036/ADR-0045 now
produce in bulk) answers container-introspection methods as the *container*
even when raku would answer as the *value*:

```raku
my @a = 10, 20;
my $p = @a[0]:p;
say $p.value.WHAT;        # raku (Int)     mutsu (Int)     -- fixed
say $p.value.VAR.^name;   # raku Scalar    mutsu Scalar    -- correct
say $p.value.^name;       # raku Int       mutsu Scalar    -- WRONG
say $p.value.VAR.WHAT;    # raku (Scalar)  mutsu (Int)     -- WRONG
```

The same two rows are wrong for any anonymous expression whose value is a cell.
Through a *named* `:=` binding they are all correct, because the compiler
special-cases `.VAR` on a named variable
(`compile_expr_method_var_on_index`, `src/compiler/expr.rs`):

```raku
my @a = 10, 20; my $r := @a[0];
say $r.WHAT, $r.^name, $r.VAR.^name, $r.VAR.WHAT;   # all correct
```

## Root cause

`src/vm/vm_call_method_ops.rs` decontainerizes a `ContainerRef` receiver for
every method except `VAR`, and then has to guess what a following `^name`/`WHAT`
meant. `.VAR` returns the cell *unchanged*, so by the time `^name` runs there is
nothing left to distinguish "this cell was reached through `.VAR`, report the
container" from "this cell is simply an aliased value, report the value".

The current compromise (2026-08-27) intercepts `^name` only — that keeps
`.VAR.^name` answering `Scalar`, which is the form roast actually uses (seven
whitelisted files), while letting the much more common bare `.WHAT`
decontainerize. `WHAT` was moved out of the intercept because ADR-0036 slice 3
and ADR-0045 slice 4 hand element containers out in bulk, so
`@a.pairs[0].value.WHAT` would otherwise have started answering `Scalar` where
it answers `Int` today.

## The real fix

`.VAR` needs to return a value that is distinguishable from a bare cell — a
distinct "container view" rather than the cell itself — so that `^name`/`WHAT`
can answer from the view and a bare cell can always decontainerize.

The obvious candidate, wrapping the cell in `ValueView::Scalar`, is **already
taken**: ADR-0040 slice 1 uses `Scalar(ContainerRef(..))` for a reference-pushed
element (`@a.push(@b)`), and `vm_call_method_ops.rs` treats that shape as
transparent for everything except the renderers. So this needs a genuinely new
representation (or a dispatch-time flag paired with the `CallMethod "VAR"`
emission at `src/compiler/expr.rs:482-491`), which is why it is a ticket rather
than a fix folded into the producer slices.

## Related

- `t/subscript-pair-element-container.t` already `todo`s a neighbouring gap:
  `(@a[0]:kv)[1].VAR.^name` does not see the cell at all, because the anonymous
  computed index target goes through the general read chokepoint which
  decontainerizes before `.VAR` runs.
- ADR-0036 §5 Q4 predicted exactly this failure mode ("a leaked `ContainerRef`
  surfaces as a wrong `.raku`/`.WHAT`/`.gist`"); this ticket is the residue of
  that question after slices 2-3.

## Reproduce

The four `say` lines above, no fixtures.
