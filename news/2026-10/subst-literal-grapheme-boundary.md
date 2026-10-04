# Literal-string `.subst` respects grapheme boundaries

`Str.subst` with a literal string pattern used `str::replace`, so it matched in the middle of a
grapheme (`"ｶﾞ".subst("ｶ", "Y")` gave `Yﾞ`; Rakudo leaves it unchanged). It now shares one
grapheme-boundary-checked scan with `.index`. Found via `Lang::JA::Kana`, whose half-width to
full-width katakana conversion now passes both test files.
