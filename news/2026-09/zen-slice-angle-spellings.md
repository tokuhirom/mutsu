# Guillemet and double-angle zen slices keep the whole container

Empty angle subscripts are zen slices in all three spellings: `<>`, `«»`, and
`<<>>`. mutsu already handled the plain angle spelling, but parsed the two
interpolating spellings as empty key lists, so `%hash«»` and `%hash<<>>`
returned no values instead of the whole hash. Their dotted spellings had the
same gap.

The parser now shares the zen-slice expression and `:k` / `:v` / `:kv` / `:p`
adverb mapping across all three postcircumfix arms. Non-empty interpolating
subscripts continue to use their existing quote-word semantics.

Pinned by `t/collections/subscript/zen-slice-angle-spellings.t`.

Closes #8205.
