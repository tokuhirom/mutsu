# A method call no longer copies its writes back into the caller's env

A method's frame is an overlay on its caller's env. When the method returned,
`merge_method_env` walked that overlay and copied every write the caller could
see back into the caller's env, filtering out the frame's own parameters,
locals, attributes and statics name by name. Named subs and closures lost the
same return copy in ADR-12529 slices 41-43; this retires it for methods
(#18274, slice 3 of ADR-18244's O1 stage).

A census with the merge switched off found three kinds of code that still
depended on it. Each reached a variable of the method's declaring scope only by
name, so each now gets the variable's cell instead:

- **A scalar spelled like a class.** In `my $C = 0; class C { method foo { $C } }`
  the method read the type object `C`: a scalar's env key has no sigil, and the
  class registration binds its type under the same key. The capture a method
  takes of its declaring frame dropped uppercase names as type-shaped; a name it
  captures from the frame's own slot is a variable, and is now kept.
- **A destructuring declaration before a class that composes a role.** Such a
  class joins the BEGIN prologue, so it was compiled before `my ($a, $b) = ...`
  declared `$a`, and a method writing `$a` could not give it a cell. The
  prologue now splits a destructuring declaration as it splits `my $x = ...`:
  the declarations move to the prologue and the assignments stay in place
  (roast `S14-roles/conflicts.t`).
- **A method called while its class body runs.** A body `my` that a method
  writes or rebinds is now declared in a cell, a binding cell for a rebind, as
  an outer variable a named sub writes already is.

With these, all of `t/` and the roast whitelist pass with the merge off, and the
merge is deleted along with its key predicates and per-class drop cache.

On the AttrX::Mooish `t/090-concurrent.custom` probe (subtest 1, one worker),
callgrind's per-round slope (R = 120 to 360 rounds, profiling build) fell from
6,097,786 to 5,762,273 instructions (-5.5%). Wall clock, 8 interleaved runs of
1,000 rounds per side: 3.40 to 3.20 ms CPU per round at one worker (-5.8%), and
4.99 to 4.75 ms at twenty (-4.8%, 6 runs of 100 rounds per worker). The
MCP::Server::Tool::Web `html-to-text` probe is neutral (7.44 to 7.24 s CPU at
scale 0.3, within its noise).
