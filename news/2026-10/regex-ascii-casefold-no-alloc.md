# `:i` matching folds ASCII without allocating

Every `:i` literal attempt case-folded both sides through `casefold_char`,
which upper- and then lower-cased each character via two `String`s and a
`Vec`. An ASCII character's fold is simply its ASCII lowercase, so the fold now
answers it with no allocation and keeps the general conversion for everything
else. A small `Schema::Core` parse in YAMLish (`:i` tokens such as
`'.inf'`, `'null'`, `yes`) went from ~553k to ~527k instructions (−4.6%).
