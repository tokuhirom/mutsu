# BigRoot: rw lvalue returns and big-FatRat typing

The `BigRoot` distribution (arbitrary-precision roots by Newton's method on
`FatRat`) was drawn by the ecosystem roulette with all three of its test files
red: two died with "Cannot modify an immutable Int (30)" and one with a
`returns FatRat` type check receiving `Any`. It exposed six independent
interpreter bugs, all fixed generally.

**Private rw methods as assignment targets.** `self!result($n, $r) = $v`, where
`!result` is an `is rw` private method, never ran the method. The instance
form was treated as the trusted private-*attribute* store (`$a!A::foo = v`)
and wrote an attribute named `result`; the type-object form (BigRoot's methods
are all called on the class) fell to the legacy setter chain. Both reported
success and wrote nowhere. A declared private method now takes the same lvalue
path as a public one: run it once and write through the container it hands
back.

**Declaration and assignment tails.** `method precision is rw { state Natural
$precision = 30; }` returned the value 30, not `$precision`'s container,
because only a bare *expression* tail was compiled in container mode. A tail
`VarDecl` or `Assign` statement in an `is rw`/`is raw` routine now boxes the
variable's cell exactly as a bare `$precision` tail does. The six hand-copied
tail arms that produced the value collapsed into one helper.

**`state` inheriting a caller's readonly mark.** The readonly registry is keyed
by name, and a `state` declaration only runs its initializer once, so
`sub test-root(:$precision) { BigRoot.precision = $precision }` made the
callee's own `state $precision` readonly — any store to it died. A `state`
declaration now clears the mark on every call (journaled per frame, so the
caller's parameter is readonly again on return). The same bug refused
`sub p { state $x = 1; $x++ }` when called from `sub t($x)`.

**Plain `=` from an rw routine aliased the source.** `my $t = f()` where `f` is
`is rw` and ends in a not-yet-existent hash element stored the deferred
`HashEntryRef` token itself, so `$t = 7` wrote the hash. The expression form
`(my $t = f())` (a `SetGlobal`) kept a `ContainerRef` as well. Both stores now
read through the token, as rakudo copies there. The `with`/`without` topic
temporary deliberately keeps the container (`without f() { $_ = 5 }` writes the
slot), so the method call it makes (`.defined`) now reads a `HashEntryRef`
invocant through instead of reporting it defined.

**Big FatRats kept their type only sometimes.** Once a `FatRat`'s numerator or
denominator outgrows 64 bits it is a `BigRat` with a FatRat flag, and `abs`
and `**` rebuilt it without the flag — a `Rat`, which then degraded to `Num`
past uint64. And `.round($scale)` always produced a `Rat` for a rational
scale, where Rakudo's `(self / $scale + 1/2).floor * $scale` has the type of
`Int * $scale`: a FatRat scale gives an exact FatRat (40 digits stay 40
digits).

BigRoot 0.1.1 goes from red (0/3 baseline files) to green. Pins:
`t/oo/method/private-method-rw-lvalue.t`,
`t/routines/rw-routine-tail-declaration-container.t`,
`t/types/fatrat-round-abs-pow-keep-type.t`.
