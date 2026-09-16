# Str.subst preserves regex start anchors

Fixed native `Str.subst` so a leading `^` in a regex literal remains anchored
when ordinary regex whitespace wrappers surround the first atom.
