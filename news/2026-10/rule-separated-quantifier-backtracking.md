# Let separated rule quantifiers backtrack before significant whitespace

In a `rule`, a separated quantifier such as `<word>+ % <separator>` can now give
back its last item when a later term needs it. The rule whitespace pass marks
the quantifier as backtrackable when significant whitespace follows the
separator, while preserving an explicit ratchet modifier. This also covers
bounded quantifiers and `%%` separators.
