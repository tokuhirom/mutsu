# A scalar store asks its own frame's overlay for a parked cell, not the whole env chain

The compiled scalar-store fast path (`exec_set_local_scalar_fast`) runs one probe
before it commits: is a shared cell or a Proxy parked in env under the slot's name,
which the full path would adopt and write through? It asked the whole env chain
(`Env::get_for`), so a store's cost grew with the number of names every enclosing
tier holds, which is what a loaded module such as Cro adds (a miss walks all of
them). About 310 Ir on every plain scalar store with Cro loaded (#7817).

A slot is this frame's own variable, and the `GetLocal` read already asks only the
frame's own overlay for the same thing (`local_cell_adoption_target`, ADR-0097
§15: a same-named container in a caller's tier is another variable and must never be
adopted). The store now asks the same question the same way. A caller's cell under
the name used to decline the fast path for a store that must not touch it anyway;
it no longer does. `t/vm/writeback/scalar-store-ignores-callers-same-named-cell.t`
pins that a callee's stores to its own `$c` neither write through nor disturb a
caller's captured `$c`, in loops, recursion and nested routines.

`$s += %h<a>` in a loop (profiling build, callgrind with collection around the loop
only, slope over 3,000 to 9,000 iterations): with `use Cro::HTTP2::RequestParser`
loaded 2,933 -> 2,659 Ir per iteration (-9.3%); without it 2,178 -> 2,158 (-1%).
