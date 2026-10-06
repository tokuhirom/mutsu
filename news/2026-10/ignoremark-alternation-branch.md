# Branch modifiers survive literal alternation

Literal alternation no longer merges branches with their own `:m` or `:i`
modifier into a character class. This preserves mark-insensitive and
case-insensitive matching for the individual branch.
