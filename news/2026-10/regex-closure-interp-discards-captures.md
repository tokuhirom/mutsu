# Regex `<{ code }>` no longer merges the interpolated pattern's captures

The pattern produced by a `<{ code }>` closure interpolation is matched as its own
regex, as in Rakudo: its positional and named captures are discarded instead of being
merged into the caller's `$/`. `"a12b" ~~ / a <{ '(\d)(\d)' }> b /` now gives a match
with no captures. Pinned by `t/regex/regex-closure-interp-discards-captures.t`.
