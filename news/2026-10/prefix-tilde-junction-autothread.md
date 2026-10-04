# Prefix `~` autothreads over a Junction

`~(1|2)` now yields a Junction of Strs (as Rakudo does) instead of stringifying the whole Junction into one Str. Fixes #11759.
