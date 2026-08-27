# An unforced lazy list stored in a `$` scalar loses its itemization

`my $a = (gather { take 1; take 2 }).List; say $a.raku` prints `(1, 2)` in
mutsu and `$(1, 2)` in raku. Assigning a List to a `$` scalar itemizes it, and
`.raku` renders an itemized list with the `$(...)` prefix — mutsu does this
correctly for an ordinary List (`my $x = (1, 2); $x.raku` is `$(1, 2)`), and
for the two-statement gather spelling (`my $g = gather {...}; my $b = $g.List;
$b.raku` is `$(1, 2)`), but not for the inline one.

## Root cause

`(gather ...).List` does not force the gather coroutine: the `.List` dispatch
intercept (`src/vm/vm_call_method_ops.rs`, the "gather-list-context" arm)
returns the SAME `LazyList` with `with_list_context()` set, so the value stored
into `$a` is still an unforced `ValueView::LazyList`. Whatever machinery marks
a value itemized at scalar-assignment time therefore never sees a List. The
List only materialises later, per method call, inside the dispatch paths that
force it (`vm_call_method_ops.rs` / `vm_call_method_mut_ops.rs`), and those
rebuild a bare `Value::array(items)` with no itemization.

A fix has to decide *where* the itemization lives: either the scalar-assignment
site records "this slot is itemized" on the `LazyList` (a third context flag
next to `array_context` / `list_context`), or the forcing sites consult the
container the value was read from. The first is probably right and mirrors
`with_list_context()`, but it touches the same context-flag family ADR-0038
phase 4 promoted off env strings, so it wants a look at that design first.

## Repro

```raku
my $a = (gather { take 1; take 2 }).List;
say $a.raku;   # mutsu: (1, 2)      raku: $(1, 2)
my $g = gather { take 1; take 2 };
my $b = $g.List;
say $b.raku;   # mutsu: $(1, 2)     raku: $(1, 2)
```

## Why it is only a ticket

The *type* is now right (`.^name` is `List`, `.raku` no longer renders `.Seq`,
and `eqv` against a List answers True — all fixed in the 2026-08-28 slice of
`todo/deep/vendor-real-test-module.md`). Only the `$(...)` rendering differs,
which shows up in `.raku`/`.perl` output and in `is-deeply` failure
diagnostics, not in the comparison result. `t/lazy-list-eqv-and-list-view.t`
pins the fixed half and carries a comment pointing here.
