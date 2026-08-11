# ADR-0025 slice 1: Instance-holding captured scalars get cells; inline protect executors install the block's upvalue array

The Cro "check 4" family (closure captures hijacked by same-named caller
lexicals) and the http-session staleness mirror were re-diagnosed with
in-repo, dependency-free repros, sharpening the deep ticket's root cause:
the failing `$encoder` IS captured-and-mutated and its check closures ARE
escaping-deemed — every defense fired except the **value-kind skip** in
`box_captured_lexicals` / `box_decl_local_cell`, which refused to box a
slot currently holding an `Instance`. HPACK encoders, `Instant`, and
session objects are all Instances, which is why both failure directions
(hijack and staleness) hit this family exclusively. The skip's original
#2749 rationale is obsolete: instances now mutate in place through their
Gc-shared attr cell, and "a cell holding an Instance" has long been an
exercised state (assign an object into an already-boxed scalar).

ADR-0025 (`docs/adr/0025-captured-scalar-cells-value-kind-blind.md`)
records the decision: slice 1 removes `Instance` from the skip (two lines);
slice 2 (planned) retires the escape verdict as a correctness gate via
decl-site cells for every vouch-refused captured scalar; slice 3 items
(type-constrained scalars, itemized `$`-held containers, `@`/`%`/`&`) stay
enumerated with their blockers.

Slice 1 flushed out a latent VM bug within hours — the designed safety-net
behavior: `Lock.protect`'s inline executors (`exec_protect_block_inline`,
`call_protect_block`) ran the protect block's bytecode without installing
the block's own captured upvalue array, so a `GetUpvalue` in the block
indexed the ENCLOSING closure's array; with `$l` newly boxed, the protect
block `{ $r += $i }` read the Lock cell as `$i` and accumulated nothing
(`t/lock-protect-shared-scalar.t`). Both sites now swap the correct array
in; the remaining inline-exec sites are ticketed in
`todo/tickets/inline-closure-exec-sites-skip-upvalue-array-install.md`.

Results: `http2-request-serializer.rakutest` fully green (notok 3 → 0);
`http2-response-serializer.rakutest` 3 → 1 and `http2-request-parser.rakutest`
1 → 1 (both residuals re-diagnosed as different shapes — narrowed in the
deep ticket); the session-expiry staleness mechanism is pinned by
`t/closure-capture-instance-cell.t` (6 tests, raku-validated), while the
session files themselves are blocked behind a pre-existing rc=139 crash on
main (`todo/tickets/http-session-tests-crash-rc139-on-main.md`). The
loop-param hijack ticket gained a verified root cause (slotless for-loop
params are invisible to the free-var analysis; body reads become
GetUpvalue and bypass the loop binding).
