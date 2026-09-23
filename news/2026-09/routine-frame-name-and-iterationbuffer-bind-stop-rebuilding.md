# Two per-call rebuilds gone: routine frame names and IterationBuffer binds

Profiling an empty `[]` element of a JSON::Fast decode after ADR-0115 put two avoidable
rebuilds near the top.

**Every routine call re-split its own name.** `push_routine_with_location` stripped a
package qualifier from the routine's name with `name.as_str().rsplit_once("::")` on every
call. That builds a `StrSearcher`, which constructs a Two-Way search state, whether or not
the name has a `::` in it. This path serves every routine frame: the light call paths,
TRIR bodies (which run under their own frame since ADR-0112 Step 3's second slice), and
the general entry. It now asks `qualified::is_qualified` and `qualified::unqualified_part`.
Both are decided once per symbol, which is the rule CLAUDE.md sets for names derived from
other names.

**Binding an `IterationBuffer` copied its whole attribute map.** Rakudo's `List` keeps its
elements in a `$!reified` buffer. `nqp::bindattr(@r, List, '$!reified', $buffer)` makes the
two share one store: mutsu re-points the buffer at the array's node, and vivifies an empty
buffer's store on first use. Both did it by cloning the instance's attribute map with
`to_map()`, inserting one key, and committing the whole map back. They now insert the one
key into the shared cell, as `value_buf` already writes its storage attribute.

Measured on a release build in a 4-core container:

- empty `[]` element: ~37K → ~34K instructions (callgrind);
- 727-record SPDX `from-json`: ~0.146 s → ~0.133 s.

What an empty `[]` still costs is spread thinly. About a quarter is the `nqp::` dispatch
table (`bindattr`, `create`, `push`, `p6scalarwithvalue`). About a tenth is re-reading
`nom-ws`'s free `$ws`: it is `:=`-bound to a list rather than held in a cell, so the TRIR
outer cache cannot keep it. The rest is TRIR frame and routine-frame bookkeeping. The
interpreter-level cuts left are each a few percent; the next multiple is ADR-0112 Step 4.
