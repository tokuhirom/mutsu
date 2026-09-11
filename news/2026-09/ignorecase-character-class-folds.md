# Ignorecase character classes preserve expanded and composed entries

Character classes under `:i` now retain entries whose Unicode case fold expands
to multiple characters, such as `ß` and `ﬀ`, without matching a partial fold.
Base characters followed by combining escapes are also composed into one class
entry, so the class matches the grapheme rather than its bare base character.

This restores the affected `roast/S05-modifier/ignorecase.t` cases and adds
regression coverage in `t/regex/syntax/regex-ignorecase-charclass-folds.t`.

Fixes [#7907](https://github.com/tokuhirom/mutsu/issues/7907).
