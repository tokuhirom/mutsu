# `start` honors a dynamic CurrentThreadScheduler

`start { ... }` under `my $*SCHEDULER = CurrentThreadScheduler.new` now cues its body through that
scheduler, whose `.cue` runs inline, so the promise is already `Kept` (or `Broken`) when `start`
returns, as in Rakudo. Previously the block went to the worker pool and the promise was still
`Planned`.
