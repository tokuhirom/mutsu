# `$!r := $local` is visible to `self.r` in the same method

Red's `MetamodelX::Red::Model.compose` does

```raku
$!rs-class := create-resultseq($rs-class-name, type);
type.WHO<ResultSeq> := $!rs-class;
die "..." unless $.rs-class ~~ Red::ResultSeq;
```

and every Red model died at load time with `Any should do the Red::ResultSeq
role`: `$.rs-class` still read `Any`. #9498 first pinned it on the type
object's `::`-qualified name, but the name was a red herring -- any bind of a
*local variable* to a scalar attribute showed it:

```raku
class H { has $.r; method go { my $x = 42; $!r := $x; say self.r } }
H.new.go;   # raku: 42, mutsu: (Any)
```

A method keeps its scalar attributes in local slots and mirrors each write
into `self`'s attribute storage right away, so an accessor called on `self`
sees it. `$!r := $x` leaves `$x`'s container (a `ContainerRef`) in the slot, and
the mirror deliberately skipped container values -- writing one *through* the
attribute's own container would nest it -- so the instance kept the old
container until the method returned and the exit writeback caught up. A bind
of a literal (`$!r := 43`) never had the problem because the slot then holds a
plain value.

The mirror now seats the bound container itself in `self`'s attribute slot
(a rebind, not an assignment through the old container), which is exactly
what `:=` means. Red's models get past the `ResultSeq` check and reach the
next, independent blockers. Pinned by
`t/oo/attribute/attr-bind-local-visible-to-accessor.t` (#9498, part of #7988).
