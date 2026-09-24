# A `:=` rebind no longer re-binds a variable bound to the old name

`my $a := [1]; my $f := $a; $a := [2,3]; say $f` printed `[2 3]` in mutsu;
raku prints `[1]`. `my $f := $a` binds `$f` to what `$a` holds at that moment,
and a later `$a := ...` changes only `$a`'s binding (#9207).

The whole-container bind (`my $f := $a` with `$a` holding an Array) puts both
names into one shared `ContainerRef` cell, and records that cell in the env
under both names. The later rebind `$a := [2,3]` correctly replaced `$a`'s local
slot with the new array, but its store takes the slot-authoritative path that
skips the env mirror, so the env entry for `a` kept naming the old shared cell.
The next by-name sync (every `say` republishes live locals into env) then wrote
the new array *through* that cell — preserving container identity, as it should
for an assignment — and so replaced `$f`'s contents too. The plain-value case
(`$a := 5`) wrote `5` into the same cell, which a closure over `$f` then saw.

`exec_set_local_op_inner` now treats a rebind whose env entry is a shared cell
as replacing that entry outright rather than writing through it. Pinned by
`t/vm/binding/rebind-does-not-rebind-earlier-alias.t` (mainline, inside a sub,
a plain-value rebind, a closure over the alias, and the write-through that a
`my $f := $a` over a real Scalar container must still keep).
