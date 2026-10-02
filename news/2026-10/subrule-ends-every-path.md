# A subrule call runs the caller's continuation once per path

When a subrule reached the same end position by two different paths
(`regex d { a || a || ab }`), mutsu kept only the first path and dropped the
second, so a code block or action after the call (`<d> { ... } 'x'`) ran once
for that end instead of once per path, and a later path's captures were never
seen by the continuation. Rakudo enters the caller's rest once per path.

The deduplication lived in three places that had to agree: the tree walk's
streamed subrule call (`drive_named_subrule_candidates`), its eager end set (the
non-LTM `Named` arm), and the compiled engine's per-frame `seen` list, which
copied the walk's behaviour for the differential mode. All three are gone; the
proto/LTM candidate tie-break is unchanged. The grammar benchmarks' timed
sections are unchanged (JSON::Tiny 0.0286 s before and after). (#10489)
