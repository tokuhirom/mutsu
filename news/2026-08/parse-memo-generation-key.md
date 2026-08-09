# ParseMemo keys now include a parse generation, closing a stale-pointer soundness hole

The parser's three memo tables (`EXPR_MEMO`, `STMT_MEMO`, `PRIMARY_MEMO`) and the
sibling `STMT_ANON_STATES_TLS` table keyed cached parse results by the raw
`(ptr, len)` of the input `&str`. That key only identifies a slice while its
owning buffer is alive — but nested parses do not respect that lifetime:
`scan_module_source` reads a module file into a temporary `String`, parses it
with `parse_program_partial` (populating the memo tables with entries keyed by
pointers into that buffer), and the `String` is then dropped mid-way through the
*enclosing* parse. When the allocator handed the freed address to a later
same-sized allocation, a stale entry from the dead buffer could be returned for
unrelated input — a silent false success or false failure with no error at all.
Because the collision depends on the allocator's address layout for a given
binary, the symptom was a per-build lottery: deterministic for one binary,
absent for the next (see
`todo/deep/pointy-block-custom-param-trait-parse-time-check-fails-for-large-modules.md`,
where `http-router.rakutest` flipped between 64/83 and 0/83 across rebuilds
with no source change).

The fix mixes a thread-local, monotonically increasing **parse generation**
into every memo key: `(generation, ptr, len)`. `parse_program` and
`parse_program_partial` each enter a fresh generation on entry and restore the
enclosing parse's generation on exit (RAII guard, so early error returns are
covered). Within one generation every live buffer's `(ptr, len)` is unique;
entries from other generations never match, so a nested parse's entries can
never leak into the outer parse via address reuse. The generation is constant
throughout a single parse, so memo hit rates are unchanged — the cost is one
thread-local read per lookup and 8 bytes per key.

Pinned by a unit test (`parser::memo::tests::generation_isolates_entries_at_the_same_address`)
that stores entries for the same slice under two generations and verifies
neither side can see the other's entry.

Whether this was the root cause of the `http-router.rakutest` flip is not yet
proven (the session's builds were all on the "lucky" side, so no failing binary
was available to test against). Four release rebuilds with deliberately varied
binary layout (dead-code padding in `main`) all held stable at the historical
maximum of 64/83 after the fix. The deep ticket stays open with an updated
decision protocol: if the flip ever recurs on a fixed binary, the memo theory
is refuted and the "full CLI path divergence" investigation becomes the main
line.
