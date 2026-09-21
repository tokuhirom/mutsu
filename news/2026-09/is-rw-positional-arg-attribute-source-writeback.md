# `is rw` positional argument now writes back through a caller's `$!attr`/`$.attr`

An `is rw` positional parameter bound to a caller's instance-attribute
expression (`$!buf` or `$.buf`) silently lost the mutation whenever the call
target was anything other than literally `self` — and even the `self` case
only "worked" by an unrelated accident. Reported as
[#8904](https://github.com/tokuhirom/mutsu/issues/8904).

## Root cause

Passing `$!buf` as an `is rw` argument had no producer of the attribute's
*container*: `compile_expr_var` compiles `$!attr` to a plain `GetLocal` of
its seeded local slot (the method-entry attribute snapshot), so the value
handed to the callee was always a copy, never a shared cell. The exit-time
rw-writeback loop knew this and explicitly skipped writing an
attribute-shaped source name back (`is_attr_twigil_shaped(source_name) =>
continue`), on the assumption that a shared `ContainerRef` cell already
existed for it — an assumption nothing actually established for this
argument shape.

Worse: a separate, unrelated mechanism (`write_self_attr_cell`, meant for a
sigilless parameter aliasing a *same-class* `has $x` attribute) reused the
identical alias-key encoding an `is rw` parameter's "so `:=` can resolve
through it" bookkeeping also writes. So an assignment inside the callee
(`$input .= subbuf(1)`) walked that alias chain and wrote into the CALLEE's
own `self` attribute cell under the same bare name — a coincidental match
when the call target happened to be `self` (same instance as the caller),
but a silent no-op when the callee had no such attribute, and active data
corruption of an unrelated object's same-named attribute when the callee's
class happened to declare one too:

```raku
class Foo {
    has Buf $.buffer = Buf.new(1,2,3,4,5);
    method chop-self(Buf $input is rw) { $input .= subbuf(1); }
    method steal(Foo $other) { $other.chop-self($!buffer); }
}
my $a = Foo.new;
my $b = Foo.new(buffer => Buf.new(9,9,9));
$a.steal($b);
# before the fix: $a.buffer unchanged, $b.buffer clobbered to (2 3 4 5)
# after:          $a.buffer is (2 3 4 5), $b.buffer still (9 9 9)
```

## Fix

Extended ADR-0067's existing rw-argument-container producer/consumer pair
(`MarkRwArgRefContext`/`MarkRwArgRefContextCallee`, previously reserved for
an accessor-call-shaped argument like `f($c.v)`) to a bare `$!attr`/`$.attr`
Var argument too. A new opcode, `ResolveAttrRwCandidate`, follows the plain
`GetLocal` read of such an argument and — only when the runtime marker
confirms the resolved callee actually binds that position to the caller's
container — pops the plain value and replaces it with the attribute's own
shared `ContainerRef` cell (the same primitive `AttrContainerRef` already
uses for a `return-rw $!v` tail). The marker is gated on the callee's
signature exactly like the accessor case, so the overwhelming majority of
calls (no `is rw`/`is raw` parameter involved) pay nothing and read the
identical plain value as before.

With the argument now a genuine shared cell, the existing exit-time
"skip an attribute-shaped rw-writeback source" logic is correct rather than
a leap of faith, and `write_self_attr_cell`'s stray alias-chain write no
longer has anything incorrect to paper over for this shape.

Regression test: `t/oo/attribute/rw-arg-attribute-source-writeback.t`.
