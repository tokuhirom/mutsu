# `reverse` handles finite `gather` sequences

`reverse gather for ...` now materializes a finite lazy sequence before
reversing it, matching Rakudo instead of returning an empty sequence.
