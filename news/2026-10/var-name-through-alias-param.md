# `.VAR.name` through an aliasing parameter; `PROCESS::` ignores `my $*X`

Two container-descriptor gaps kept the Tee distribution from running at all,
and the first one also held Test::Describe's `t/04-change` red (#11196).

**`.VAR.name` names the caller's variable.** A parameter that aliases the
caller's container (`is rw`, `is raw`, `\x`, named or positional) *is* that
container, and rakudo's descriptor names the variable it was declared as:
`sub f($x is rw) { $x.VAR.name }; f($a)` is `$a`, not `$x`. A shared container
cell now carries the declared name, set by the bind paths that promote a
caller variable into one. That covers the light positional `is rw` bind and
the general binder's positional and named arms, and so the name follows
relays, closures and `.new(:value($y))` into `BUILD(:$value! is raw)`. A
dynamic `f(my $*OUT)` gets no shared cell, so the binder records its name
instead. The `.VAR` reflector's per-name cache is now checked against that
name, so a second sub whose parameter has the same name no longer reuses
the first one's descriptor.

**`PROCESS::<$OUT>` is the process value.** The `PROCESS::` stash was built
from every dynamic visible on the caller chain. A caller's `my $*OUT`
therefore made `PROCESS::<$OUT>` that lexical value: Tee's constructor read
`PROCESS::<$OUT>` as the very Tee it was building, and so wrote nothing to
its file. A `my $*name` declaration now marks the binding as the frame's
own, and the stash skips it. It still shows the interpreter-seeded process
dynamics, an undeclared `$*OUT = $fh` assignment, and every
`PROCESS::<$name> = ...` install.

Tee's test file and all three of Test::Describe's now produce the same output
as rakudo.
