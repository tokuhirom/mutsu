# A sub declared in a routine reads its own activation's variables

A `my sub` declared inside a routine read its free variables by name from
whatever env was live at the call. A caller with a same-named local therefore
answered in place of the routine's own variable:

```raku
sub sh($p) { my sub t() { $p }; -> { my $p = 99; t() } }
say sh(5)();    # raku: 5    mutsu was: (Any)

sub nb($p) { my sub t() { $p }; { my $p = 9; t() } }
say nb(4);      # raku: 4    mutsu was: 9
```

A nested block, a `for` or `map` parameter, or a local of a calling closure
all shadowed it. Writes were wrong too: `$c++` inside the inner sub was replayed
into the caller's `$c`.

ADR-0024 fixed this for mainline and bare-block subs by capturing cells into
buckets keyed by the sub's name. A routine-nested sub needs one binding per
call of its routine, so those buckets could not serve it
([ADR-0114](../../docs/adr/0114-routine-nested-sub-free-var-aliases.md)). The
declaring frame now gets one hidden local per free variable of the inner sub.
Each run of the declaration binds it to the variable's cell, and the inner
sub's by-name accesses resolve through it first. Because the alias is an
ordinary local of the declaring routine, each call gets its own and the
call-return merge does not copy it out. Closures that call the inner sub
capture it the same way they already capture the sub's free variables
(mutsu#9106), and writes through it are no longer replayed into the caller.

Pinned by `t/routines/closure/routine-nested-sub-shadowed-free-var.t`
(mutsu#9111).
