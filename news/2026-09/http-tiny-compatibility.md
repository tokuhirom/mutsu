# HTTP::Tiny 0.2.6 reaches parity

The ecosystem roulette selected HTTP::Tiny 0.2.6, which now passes all 7 of
its actionable test files under mutsu. The fixes cover parameterized
`Buf`/`Blob` construction and matching, multi-parameter `for` bindings,
private nested type visibility, `with` binding and nested element writeback,
regex matches produced inside `.first`, JSON attribute itemization, and the
value path through `when` blocks.

The distribution moves from partial to green without changes to its source.
Regression coverage includes regex-match publication from matcher predicates
and writeback through a nested loop-element topic.
