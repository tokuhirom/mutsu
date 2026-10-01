# A grammar parse stops assembling its Match tree from per-call deltas

A grammar parse of `benchmarks/bench-grammar-parse-big.raku`'s 10 KB document made about 165,000
allocator calls (192,837 for the whole benchmark, 28,222 of them outside the parse), about twenty
per subrule Match it built (#10488). Almost none of them were the tree itself. The compiled regex
engine had inherited the walk's capture-delta protocol: every subrule return built a
`RegexCaptures` delta with a hash map and a node vector of its own, for the caller's level to copy
the node out of and free; a separated quantifier (`<pair>* % ','`) and a `~` goal match
(`'[' ~ ']' <list>`) matched each iteration or side in a capture level of its own, copied it twice
and folded the copies afterwards; every call allocated its frame; and a proto return copied its
`:sym` name into a freshly allocated cold payload.

[ADR-10488](../../docs/adr/10488-capture-levels-are-written-in-place.md) writes capture levels in
place instead:

- A level's named captures are a small map in filing order, holding one node per name inline
  (it was a hash map whose table and per-name vector were each allocated on first use).
- A callee frame's return files the subrule's Match straight into the caller's level, through the
  same filing decision the walk's delta builder uses (`file_named_candidate` and a `CapSink`).
- A separated quantifier or a goal match whose two sides file positional captures or markers on
  only one side, and no name on both, files in place: the fold the levels existed for would produce
  the same tree. The names it marks list-valued are interned when the pattern compiles.
- Call frames live in an arena indexed by position; a proto's ranking, a level's undo trail and an
  LTM measurement's vectors are reused.
- A capture's `:sym` and alias rule name are interned `Symbol`s, inline.

`scripts/bench-det.sh benchmarks/bench-grammar-parse-big.raku` allocations: 192,837 → RELEASE_ALLOCS
(release); instructions RELEASE_IR_BEFORE → RELEASE_IR_AFTER. The step-by-step figures are in the
ADR. `tests/grammar_parse_alloc_budget.rs` pins the per-element slope of the same grammar.

Two differences from rakudo turned up while checking goal matches against it, both older than this
change and shared by both regex engines: a `rule`'s separated quantifier accepts whitespace before
its separator (#10569), and a `<(` / `)>` marker inside a goal match's inner pattern makes
`Grammar.parse` fail (#10570).
