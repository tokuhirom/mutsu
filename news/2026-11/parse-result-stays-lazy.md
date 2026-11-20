# A grammar parse returns its Match without copying it

Every `.parse` and `.subparse` ended by rebuilding the start rule's Match.
The matcher hands back a lazy Match, and the parse added `.ast`, `.actions`,
the winning `:sym<...>` variant and the capture alias map to it by
materializing its attribute map, cloning that map with the additions and
dropping the original. That was about 11,000 instructions per parse, with or
without actions, and the copy was an eager Match, so the action walk that
followed took its slower eager path.

The parse state now goes onto the start rule's capture node (`.made` into
the node's shared cell, `.actions` into a new field of its child payload),
and the `$<name>` variables the parse leaves behind are read from the node's
named slots. The result stays lazy until something actually looks inside it.
Around it:

- the action walk skips a lazy node that has no action of its own and
  nothing left below it (its subrules dispatched as they reduced), and gives
  one that has an action an empty walk node instead of scanning for one;
- `make` stores its value under the pre-interned `made` key instead of
  allocating and interning the string each time;
- the reduction-time dispatch takes the rule name as the symbol the subrule
  filer already has;
- a by-name method call no longer views every argument three times looking
  for a lazy Seq, and its undeclared-named strip uses the method symbol its
  callers hold.

On #18207's repro (a two-rule grammar with one trivial action), a parse went
from 123.3k to 110.7k instructions with actions and from 92.5k to 81.4k
without (callgrind slopes, profiling build). `Cro::Uri.parse-ref('/page/N')`
went from 970k to 728k instructions per call and `Cro::Uri.add` from 2.02M to
1.82M. MCP::Server::Tool::Web's `html-to-text` workload moved by 0.5%, since
URI handling is a small part of it.
