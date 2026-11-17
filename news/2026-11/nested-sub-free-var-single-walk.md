# A nested sub's free-variable read walks the env once

A `my sub` declared inside a routine reads its free variables by name. The
read goes through the declaring activation's alias (#9111): a hidden local of
the declaring frame bound to the variable's cell. Finding that alias walked
the env chain once to test whether the alias was bound, then walked it a
second time to read the binding. It also compared the alias table's names as
strings, although the read site already had the name interned.

`lexsub_alias_binding` now returns the binding from the walk that found it,
and compares interned names when it has one. The answer is unchanged: it is
the first bound alias, in the same order.

Callgrind slope (profiling build, 2,000 to 4,000 iterations, second run):

| loop | before | after |
| --- | ---: | ---: |
| `$probe++ while $probe < $len`, where `$len` is the declaring routine's | 4,116 | 3,831 |
| the same with a local copy of `$len` | 1,778 | 1,787 |
| MCP::Server::Tool::Web's tokenizer shape (`is-name-char($html.substr($probe, 1))`) | 19,864 | 19,203 |

A free-variable read now costs about 2,040 instructions instead of 2,340. It is
still a by-name lookup. #17971 tracks making it slot-addressed, which is what
would bring it near a local read.
