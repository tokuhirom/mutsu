# Proposed: a staged structural plan for the per-op and per-call floor (ADR-18244)

MCP::Server::Tool::Web `t/12`'s `html-to-text` on a pathological megabyte still takes ~28 s
against raku's ~7 s. Twelve perf sessions of 1-6% slices left the profile flat. ADR-18244 measures
why, using per-construct callgrind floors and a by-family breakdown of the workload:

- straight-line statements and regex matching are already at raku's speed;
- calls cost 4-21x raku's;
- loops over native `int`, non-scalar declarations, element stores and short-lived strings cost
  4-10x raku's.

It traces this to five design causes:

- by-name lexical resolution on the hot path;
- a ~17-function call protocol;
- generic op handlers that triage rare cases on every execution;
- a grammar-action harness that emulates dynamic scope;
- late-decided operators and copied substrings.

It proposes one ordered program: the action harness first, then static lexical addressing, a lean
call frame, specialized ops, and string slices. The estimate is ~1.8-2.1x in total, and no subset
reaches 2x. Part of #18244 and #16897.
