# Preserve anonymous pointy frames in backtraces

Backtraces now keep pointy and bare block frames anonymous when reporting
their subnames. `next-interesting-index(:named)` skips those frames, and
`nice(:oneline)` identifies the named routine that encloses them.
