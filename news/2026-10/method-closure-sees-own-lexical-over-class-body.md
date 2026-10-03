# A block handed to `.map` reads its method's `my`, not the class body's

When a method declared `my $enc` (or `my &enc`) that shadowed a class-body
`my` of the same name, a block the method passed to `.map`/`.grep` read the
class body's binding. `class V { my $enc = "outer"; method b { my $enc =
"inner"; (1,).map({ $enc }).join } }` printed `outer`, and URI::Template's
`Variable.expand-value` called the class body's encoder instead of the one the
method had picked (#10651).

The inline map/grep loop recompiles the block body to by-name reads, and those
reads consult the class/package body's `my` statics before the env. That order
exists for the package's named subs. A block, though, carries its creating
routine's never-written `my`s as authoritative captures (`frame_authoritative`),
and the package-store lookup now steps aside for those names.

The original #10651 shape (a class-body `my sub` calling the class body's
`my &f` from a method with its own `my &f`) had already been fixed on `main`.
A method-local that the method also writes is captured as a cell and still
loses. It is tracked as #11718.
