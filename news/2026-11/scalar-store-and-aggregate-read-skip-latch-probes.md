# A scalar store and an `@`/`%` read stop probing the env for facts no name can have

Two checks ran on a hot path in every program that loads a module such as Cro or
`JSON::Fast`, each asking an env chain whose length grows with the names a module
puts in scope (#7817).

- **A plain scalar store** cleared a stale `__mutsu_bound_decont::<name>` marker
  (the shape marker of `my $x := @a`) once the program-wide latch said some name
  ever got one: a memoized key plus a chain walk, on every store. The marker is
  written in exactly one place (`update_bound_decont_marker`), which now also sets
  a bit for the name in a process-global, monotonic name set
  (`runtime/bound_decont_names.rs`, the twin of the atomic-name set). A store to
  a name whose bit is clear has no marker to find, so it skips the probe.
- **An `@`/`%` local read** computed whether its celled slot is still what the
  frame's env names (`slot_cell_is_env_binding`, an env walk) before the atomic-lane
  probe and the thread-clone branch that are the only readers of the answer. It is
  now computed when one of the two can use it (a lane entry exists in the process,
  or this is a thread clone).

`$s += %h<a>` in a loop, `use Cro::HTTP2::RequestParser` loaded (profiling build,
callgrind slope over 3,000 to 9,000 iterations): 2,985 -> 2,940 Ir per iteration
from the second change; the first removes the `MetaNs::key` and marker lookup from
each store (about 140 Ir per store in the collection window). Without the module
2,225 -> 2,181.
