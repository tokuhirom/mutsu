# Regex named capture axis collapsed into `NamedSlot` (ADR-0016 P4, layer 2)

Following the positional-axis collapse (layer 1), the named capture state —
`named: HashMap<String, Vec<String>>` (captured text) ‖ `named_subcaps:
HashMap<String, Vec<Arc<CapNode>>>` ‖ `named_quantified: HashSet<String>` —
became one map: `HashMap<String, NamedSlot { nodes: Vec<Arc<CapNode>>,
quantified: bool }>`, on both the accumulator and stored capture nodes.

A site inventory before coding established the two facts the design needed:
every named-text write site has a span in scope (so text-only entries could
become span-bearing leaf `CapNode`s), and the pre-P4 text/subcap maps were
NOT guaranteed aligned — the engine carried an explicit repair
(`sub_count < name_count` back-fill in `store_apply_named_capture`) plus four
divergence classes (silent-marker subcap-only entries, text-only builtin
captures, merge paths that dropped subcaps, quantified-only keys). The single
axis makes all of that structural: one node per entry, the quantified flag on
the slot, the repair deleted, and the merge paths that silently dropped
subcaps now carry full nodes.

The stored named text axis is gone with layer 1's machinery: readers derive
text from node spans through the shared subject; `$<name>` backreferences are
alloc-free span comparisons; `CodeBlockContext.named` keeps its text-snapshot
shape but is materialized from spans at its single construction site. The
Match builder's exploded `named`/`named_subcaps`/`named_quantified` argument
triple is a single map parameter, which also retired the
`make_match_object_full_q` variant, and the render loop is driven by slot
nodes instead of aligning two maps by index. Silent-action captures
(`<.foo>`) keep their marker-prefixed keys inside the same map and stay
hidden from `.hash`.

`hash_captures` (`%<name>=(...)`, accumulator-only) is untouched, and capture
names remain `String` keys — interning to `Symbol`s is a separate follow-up.

Verified: `cargo test` 628, `t/` 24923, whitelisted S05 roast (93 files), and
a full local `make roast` all pass; clippy/fmt clean.
