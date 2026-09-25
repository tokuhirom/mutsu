# ADR-0116 accepted: op bodies first, and the JSON::Fast bar stays below 1.0x rakudo

ADR-0116 recorded that the first slice of ADR-0112 Step 4 (lowering TRIR chunks to Cranelift) was
built, measured and found slower: the switch loop it removes was only ~21-33% of a record, and each
compiled chunk cost more than it saved. It proposed doing ADR-0112 Step 4 as "shrink the op bodies"
(its D2) and bringing native lowering back only under D3's conditions: the switch loop above 50% of
a record, and compilation cheaper than what it saves.

On 2026-09-25 the maintainer made two calls during a backlog triage pass:

- **The bar is fixed at below 1.0x rakudo.** #8673's close condition
  (`bench-json-fast-spdx@section+jit` < 1.0 on the bench CI) will not be relaxed to the ~1.2x that
  op-body work alone is estimated to reach. So native lowering is still on the path to the goal;
  D3 now says *when* it comes back, not *whether*.
- **Op bodies go first.** ADR-0116 is Accepted as written: the remaining ADR-0121 attribute-slot
  work, `strfromcodes` and the rest of D2 come before any native lowering, because they are what
  make native lowering pay off and they help every `nqp::`-style module, not just JSON::Fast.

#9117 (ADR-0112 Step 3) and #8673 carry comments recording the same decision.
