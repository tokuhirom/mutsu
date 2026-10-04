Grammar subrule calls now reuse the generation-aware user-method probe memo.
Repeated calls in a parse no longer walk the grammar method hierarchy for each
subrule, while methods added after a cached miss remain visible.
