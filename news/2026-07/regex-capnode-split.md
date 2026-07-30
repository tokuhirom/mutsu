# Stored regex captures split into lightweight `CapNode`s (ADR-0016 P2)

`RegexCaptures` used to play two roles at once: the regex engine's *mutable
accumulator* for the pattern run in progress, and the *immutable stored node*
behind every `Arc<RegexCaptures>` sub-capture. A stored node only needs its
span, text, dispatch metadata (`sym`/`action_name`/`ast`), and — rarely —
children; it got all 14 fields anyway: 5 `HashMap`s, 1 `HashSet`, 7 `Vec`s and
a `String`, ~600 zeroed bytes per node, moved by value through candidate lists
and cloned per complete match. For a leaf-heavy grammar parse (YAMLish produces
one leaf capture per matched character in a run of spaces) that shape was a
large part of the allocator floor a clean profile shows (~23-28%).

This lands ADR-0016 **P2**: a new `CapNode` — `matched`/`from`/`to`/`sym`/
`action_name`/`ast` plus `children: Option<Box<CapChildren>>` — is now the
stored-node type on all three sub-capture axes (`named_subcaps`,
`positional_subcaps`, quantified entries) and in the `REDUCED_SUBRULES` replay
log. A leaf collapses every child collection into a single `None`, taking a
stored leaf from ~600 bytes to under 112 (pinned by `cap_node_size_guard`).
`RegexCaptures` remains the accumulator only; conversion happens once per
stored node (`into_cap_node()`) at the ~10 sites that previously wrapped an
accumulator in an `Arc`. Fields nothing ever read through a stored node
(`hash_captures`, `positional_slots`, `positional_offsets`, capture markers,
`match_from`) are dropped at conversion instead of being carried around.

The reduce-time walk (`reduce_regex_captures_made`) and the failed-parse
action replay now recurse over `CapNode` directly; the block-running body and
the children-first descent are shared helpers (`reduce_run_code_blocks`,
`reduce_child_axes`) between the accumulator top level and stored nodes, and
`subtree_has_code_blocks` answers `false` for a leaf in one branch instead of
scanning three empty collections.

No observable semantics change. Validated locally with the full `t/` suite
(24,921 tests) and a full `make roast` run before push. Perf is to be judged
from `bench-history.tsv` on the `bench-data` branch per the standing rule —
the structural claim (leaf nodes shrink ~5x, `RegexCaptures::default()` zeroes
much less per candidate) holds regardless.

Next phases: P3 (spans + shared `MatchTarget` subject, killing the Match
builder's position search), P4 (one list per axis + interned names), P5 (lazy
`Match` materialization).
