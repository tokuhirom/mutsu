# `Range.squish` returns a Seq

`(0..3).squish` answered the Range itself through an early return in
`dispatch_squish`. It now falls through to the shared element path like
`unique` and `repeated`, so it answers `(0, 1, 2, 3).Seq` as Rakudo does;
infinite ranges stay lazy (#16038).
