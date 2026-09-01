# `sub ... is rw` returning an array/hash element (implicit return) doesn't produce a mutable container

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Routine.rakudoc:231`).

## Original repro (from the doc)

```raku
sub walk(\thing, *@keys) is rw {
    my $current := thing;
    for @keys -> $k {
        if $k ~~ Int {
            $current := $current[$k];
        }
        else {
            $current := $current{$k};
        }
    }
    $current;
}

my %hash;
walk(%hash, 'some', 'key', 1, 2) = 'autovivified';
say %hash<some><key>[1][2];
```

- raku: `autovivified`
- mutsu: `(Any)` — the assignment through the `is rw` sub's return value is silently lost.

## Minimal repro (much narrower than it first looks)

The bug is **not** about `for`-loop rebinding, sigilless `\thing` binding, or autovivification
specifically. It reproduces with a plain sigiled parameter and a single hash-element read as the
sub's last (implicit-return) statement:

```raku
sub walk(%h) is rw {
    %h<some>;
}
my %hash = some => 1;
walk(%hash) = "val";
say %hash<some>;   # raku: val   mutsu: 1 (unchanged)
```

And the same for an array element:

```raku
sub walk(@a) is rw {
    @a[0];
}
my @arr = 1,2,3;
walk(@arr) = 99;
say @arr;          # raku: [99 2 3]   mutsu: [1 2 3] (unchanged)
```

Both cases: assigning to the call result of an `is rw` sub whose implicit return value is a
hash/array element (whether or not the key/index previously existed) does not write back to the
original container. mutsu accepts the assignment silently (exit 0, no error) and just discards
it — this is a correctness bug, not a missing-feature error.

Contrast: an `is rw` sub that returns a *sigilless-bound alias to the parameter itself*
(`sub walk(\thing) is rw { thing }`) DOES work correctly — the container link survives when the
returned expression is the bound term with no indexing on the way out. It's specifically the
"index into the container as the returned expression" shape that loses mutability, whether that
indexing happens directly (`%h<some>` as the last statement) or through an intermediate `:=`
rebind (`my $b := %h<some>; $b`).

## Why this is `todo/deep` and not a ticket

This appears to be a general gap in how `is rw`'s "return a mutable container" contract is
plumbed through the compiler/VM for the *implicit last-expression return* path when that
expression is an array/hash element access — as opposed to `return-rw` (tracked separately,
see `todo/tickets/control-return-rw-not-mutable.md`, which covers the explicit `return-rw`
statement and appears to be a different code path/bug). Because `is rw` + implicit return of an
indexed element is an extremely common idiom (accessor-style routines, `walk`-style container
descent, `Buf`'s own `subbuf-rw` per-element helpers), this plausibly has a wide, currently
unmeasured blast radius across roast and real-world code — it needs a proper investigation of
how rw-ness is tracked from "last expression in an `is rw` sub body" through the call-return
path to the caller's assignment target, not a narrow patch for one shape.

## Related but distinct findings (do not conflate when fixing)

- `todo/tickets/control-return-rw-not-mutable.md` — explicit `return-rw $a` on a plain scalar
  lexical doesn't propagate mutability either; likely the same underlying rw-container-return
  plumbing gap, but confirm before assuming a single fix covers both.
- `Type/Buf.rakudoc:84` (`subbuf-rw($buf, from, len) = value` as a **function-call form**, not
  a method call) also fails to mutate — but that one is root-caused separately (an env
  identity-search lookup for the target variable, see
  `todo/tickets/subbuf-rw-function-form-lvalue-not-mutating.md`) and should NOT be assumed to
  share a fix with this ticket without checking.

## Affected files (starting point)

- `src/runtime/calls.rs` / `src/vm/vm_call_ops.rs` — return-value handling and `rw`/mutable-flag
  propagation for the implicit (fall-through) last-statement return path.
- Whatever marks a returned `Value` as "safe to assign through" (a `ContainerRef`/Proxy-like
  wrapper) for `is rw` subs — compare how it's produced for a bare returned variable (works, per
  `sub walk(\thing) is rw { thing }`) vs. an indexed element read (broken).

## Re-verified 2026-09-01 (TRIAGE regeneration)

All three shapes (hash element, array element, the doc's `walk`) still silently discard the
assignment (`1` / `[1 2 3]` / `(Any)`, exit 0). Since this file was written,
[ADR-0059](../../docs/adr/0059-is-rw-routines-return-a-container.md) ("an `is rw` routine
returns a container") landed slices 1-2 **except "Slice 2's bare-`is rw`-tail half"**: a tail with
no `return-rw` still goes through the caller-side tail re-interpretation
(`rw_sub_target_expr` / `assign_rw_target_expr`), which re-evaluates `%h<some>` in the *caller's*
frame — where `%h` is the callee's parameter and resolves to nothing, hence the silent discard
(ADR-0059 §Context, first bullet). Read ADR-0059 before starting; this ticket is that open half,
not a separate mechanism, and closes when the bare tail compiles to its container and the
re-interpretation code is deleted.
