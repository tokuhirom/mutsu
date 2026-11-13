# A pool task drops fewer GC candidates and inherits its spawner's IO dynamics

Three changes cut the fixed per-task cost of a scheduled block (`$*SCHEDULER.cue`,
`start`, `race`/`hyperize`), which ParaSeq's adaptive batcher pays once per batch
([#17337](https://github.com/tokuhirom/mutsu/issues/17337)).

## An edgeless container cell is not a cycle-root candidate

The Bacon-Rajan cycle collector buffers every surviving `Gc` drop as a possible
cycle root. A `ContainerCell` holding an immediate value — a small `Int`/`Num`, a
`Bool`, `Nil`, a type object — with no `is default` value and no `QuantHash`
weight has no outgoing `Gc` edge, so it cannot be on any cycle. A loop that
rebinds a scalar (`$limit := @a[$i]`, which mints a fresh readonly binding cell
per iteration) dropped millions of such cells, filling the candidate buffer every
few thousand iterations and triggering a stop-the-world collect that reclaimed
nothing. `Trace::currently_acyclic` now lets `Gc::drop` skip buffering a node that
has no edge at the moment it dies (Bacon-Rajan's "green" nodes, decided per drop).
On ParaSeq's degree-3 `.max(:p)` over 200,000 elements the collector count drops
from 45 to 1, and the in-task limiter loop speeds up accordingly.

## A thread inherits its spawner's `$*CWD`/`$*TMPDIR`/`$*HOME`/`$*SPEC`/…

A spawned thread rebuilt the path-valued IO dynamics from the process state on
every clone, so `indir "/tmp", { await start { say $*CWD } }` printed the process
directory where rakudo prints `/tmp`. These are ordinary dynamic variables, so a
thread now inherits the spawning scope's value, exactly as it already did for
`$*OUT`. This both fixes the wrong answer and removes ~10 `IO::Path`/`IO::Spec`
instance builds from each clone.

## `hoist_builtin_dynamics` clones the base tier only when it must

Moving the built-in dynamics into a spawned interpreter's base tier cloned the
whole base map on every clone and then discarded the clone when — as is now the
common case — the overlay held none of them. It takes the overlay entries first
and clones only when there is something to install, and interns the dynamic names
once per process instead of per clone.

Together these take `spawn_clone` on a 150-task cue loop from ~44M to ~26M
instructions.
