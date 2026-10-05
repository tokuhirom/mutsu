# Literal string patterns respect grapheme boundaries

Literal string patterns in `s///` and `.subst-mutate`, string separators in `.split`, and string
keys in `.trans` now match only at grapheme boundaries. Regex literal atoms also recognize
non-mark Unicode grapheme extenders such as half-width voiced kana marks.
