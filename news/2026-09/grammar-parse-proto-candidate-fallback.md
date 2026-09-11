# Grammar start-rule proto candidates fall through

`Grammar.parse` and `.subparse` now try the next ranked proto candidate when the
first candidate fails its full match.

Pinned by `t/grammar/grammar-parse-rule-proto-candidate-fallback.t`.

Closes #7909.
