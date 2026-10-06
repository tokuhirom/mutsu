# Seq.eager answers a List

`Seq.eager` now reifies the Seq into a `List` like Rakudo, instead of returning the Seq itself (so `.raku` no longer carries the `.Seq` suffix). Fixes #12049.
