# One unrelated `:=` no longer turns off the fast local read for the whole program

The interpreter's `GetLocal` fast path (#8332) and the JIT's inline local read
(ADR-0004 J4d) were both gated on one process-wide counter. Packing a single
`ContainerRef` cell or `Proxy` anywhere bumped it for good. After that, every
local read in every frame went through the full ~230-instruction guard chain.
`use Test` alone made 20 cells, so every test file ran without the fast path,
and so did most real programs: JSON and YAML parsing, grammars, regex captures,
anything with an escaping closure. Only the micro-benchmarks kept it.

Both fast paths already refuse a slot that itself holds a cell, so the counter
was only standing guard over one probe: an env entry naming a container that
the slot did not hold. ADR-0097 §15 turns that state into an invariant. Every
site that puts a container into a frame's env also puts it into the slot, and
debug builds assert this on every fast read. Running all of `t/` and the
whitelisted roast files with the counter switched off found seven sites that
broke the invariant. They are now fixed where they write:

- a `for` topic writeback that replaced a slot cell with a bare value;
- a `:=` inside a sub that spliced the cell into the caller's env but not its
  slot;
- EVAL boxing a caller variable;
- raw parameters of routines imported through a custom `EXPORT`;
- `for $b` over a bare placeholder name;
- a `when` block's value carrying its variable's container into a loop;
- `:=` sources that found a shadowed sibling slot by name.

On the #8748 repro, one unrelated `my @unused := @data` used to cost +21.7%
instructions with the JIT on. It now costs +1.9%. The `GetLocal` call counts
are identical, and what remains is a separate store-side latch, tracked in
#10691.
