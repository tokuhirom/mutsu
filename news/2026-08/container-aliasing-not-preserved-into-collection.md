# A scalar's container survives into a List and into a Pair written as an argument

Two doc examples showed the same shape of divergence — mutsu snapshotting a
scalar by value where raku keeps it aliased:

```raku
# traps.rakudoc:406 -- every previously-pushed tuple shows the LATEST value
my @arr;
my ($a, $b) = (1,1);
for ^5 { ($a,$b) = ($b, $a+$b); @arr.push: ($a.item, $b.item); say @arr }

# variables.rakudoc:853 -- every pushed Pair shows the FINAL $i
my @a; my @a-cloned;
sub f() { state $i; $i++; @a.push: "k$i" => $i; @a-cloned.push: "k$i" => $i.clone }
f for 1..3;
say @a;         # raku: [k1 => 3 k2 => 3 k3 => 3]
say @a-cloned;  # raku & mutsu: [k1 => 1 k2 => 2 k3 => 3]
```

## What raku actually does (measured, not assumed)

The rule is not "collections alias". It is that **an `Array`/`Hash` element is
its own `Scalar` container, so a store into one copies**, while a `List` and a
`Pair` are built by *binding* whatever they are given, so they keep the source
container. Probed directly:

| program | raku |
| --- | --- |
| `my $x=1; my @a; @a.push($x); $x=5; @a` | `[1]` — copied |
| `my $x=1; my @q=0; @q[0]=$x; $x=5; @q` | `[1]` — copied |
| `my $x=1; my $l=($x,2); $x=5; $l` | `$(5, 2)` — aliased |
| `my $x=1; my %h=a=>$x; $x=5; %h` | `{:a(1)}` — copied (the hash *store* copies) |
| `my $x=1; $x.item =:= $x` | `True` |

mutsu already agreed on almost all of this. The two things it got wrong were the
two things the doc examples exercise, and they turned out to be **two independent
causes**, not one.

## Root cause 1 — `.item` was compiled as an ordinary method call

Raku's `method item(Mu \SELF:) is raw { SELF }` hands the invocant's container
straight back; `.item` only stops list flattening, it never copies. mutsu
compiled `$a.item` as a normal method call, and the method-call path
decontainerizes the invocant before the native `"item"` handler ever sees it —
so the container was already gone, and `($a.item, $b.item)` snapshotted where the
bare `($a, $b)` form (which has an explicit `WrapVarRef` arm) aliased correctly.

**Fix.** A new compiler helper `Compiler::scalar_container_alias_name`
(`src/compiler/expr_call.rs`) answers "which plain scalar lexical's container does
this expression denote", recognising both `$x` and `$x.item`. The two
container-capturing sites — the `ArrayLiteral` (List literal) element arm
(`src/compiler/expr.rs`) and the fat-arrow Pair value arm
(`src/compiler/expr_binary.rs`) — now go through it, so `.item` reaches the same
`WrapVarRef` capture a bare variable already did.

## Root cause 2 — a positional Pair argument had its container capture suppressed

`suppress_pair_capture` exists for **named arguments**: `.new(prefix => $dir)`
passes its value by the call's binding rules, so capturing the container there
breaks callees that read the bound value without a deref (that is what its field
doc says). But it was switched on for *every* call/method argument, including a
non-bareword-keyed fat-arrow written directly as data:

```raku
@a.push: "k$i" => $i;     # PositionalPair -- data, not a named argument
```

mutsu's parser already distinguishes the two (`Expr::PositionalPair` wraps
exactly the non-bareword-keyed form; a bareword key mints a named argument
instead). The two argument compilers (`src/compiler/helpers_call_args.rs`) now
suppress pair capture only when the argument is *not* a `PositionalPair`, so a
data pair keeps its value's container exactly as the standalone literal
`my $p = ("k" => $v)` already did.

The bareword form stays a named argument, which is also what raku does — and in
raku `@a.push(k => $x)` therefore pushes *nothing at all*, since `push` has no
named parameters.

## Verified

`t/attribute-container-identity.t` (green under both `raku` and mutsu) pins:
`$x.item =:= $x` in both operand orders; a List built from `.item` and from a
bare variable holding the source container in both orders; the traps.rakudoc
Fibonacci trap producing `3-5 3-5 3-5`; the pushed Pair's value being *the*
source container (`@a[0].value =:= $i` both ways) while `.clone` breaks the
alias; and the negative half — `@p.push($x)` and `@q[0] = $y` copying into a
fresh `Scalar` element, and a hash-constructor value being copied.

## Known remaining gap

A `PositionalPair` nested one level deeper inside an argument
(`@a.push((("k" => $x),))`) still snapshots, because the suppression is lifted
only for a *top-level* `PositionalPair` argument. The same applies to
`my $l = ($x,2); $l[0] = 99` (assigning through a List element), which raku
allows and mutsu rejects with "Cannot modify an immutable List" — that one is the
List-element-container surface tracked by
`docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md`, not this
ticket.
