# Match Rakudo's Seq.skip pattern order

Under `use v6.e.PREVIEW`, multiple `.skip` counts now produce values first and
then skip values, consistently for explicit arguments and lazy argument streams.
Earlier language versions reject multiple counts; one-count calls retain their
skip-first behavior.
