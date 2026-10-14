# A package block no longer copies the whole frame's locals

`OpCode::PackageScope` (a `package`/`module`/`class` block run inline) saved a
copy of every local slot of the enclosing frame on entry, restored all of them
on exit, and re-synced every env-mirrored slot from the env. In a mainline with
thousands of lexicals each package block paid for all of them, even when its
body named none (#9171).

The compiler now records, per `PackageScope`, the slots the body reads or
writes by name or by index plus their same-named peers
(`EnvConsumerSlots::package_scope_lists`), and the exit saves, restores and
re-syncs only those. A slot the body cannot reach is left as it is, so a write
made by a sub or closure the body calls survives too.

2000 package blocks in a loop under a mainline of 2000 / 4000 lexicals went
from 0.40 / 0.68 s to 0.059 / 0.051 s (debug build): the op no longer scales
with the frame's locals.
