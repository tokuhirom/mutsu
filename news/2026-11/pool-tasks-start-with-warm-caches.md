# Pool tasks start with warm caches

Every task the scheduler runs (`$*SCHEDULER.cue`, `start`, `Promise.start`) gets an
interpreter forked from its spawner, and that fork used to start with empty resolution caches.
A task running the same code as the task before it therefore re-derived every method
resolution, constructor plan, MRO and compiled call target, and re-derived each frame-lexical
`my sub` through the whole registration path. ParaSeq tunes its batch size so a batch takes
about a millisecond; with ~1 ms of cold-start cost per batch the size never grew, and a
200,000-element `hyperize(16, 3).map({ $_ * 2 })` ran as 12,000 tiny batches.

Two changes (ADR-17337):

- Frame-lexical routine derivations are shared by every interpreter of the process, keyed by
  their compile-time identity; the per-interpreter table stays the first level.
- A pool worker hands the caches of its last task to its next one, when the previous task left
  the registry it was forked with untouched and the new task was forked with that same registry.
  The fork itself is still per task, so all program state stays the task's own.

That map went from 6.5 s to about 1.7 s (rakudo 0.2 s). ParaSeq's `t/01-basic` went from 29 s
to 9 s, `t/47-deepmap` from 59 s to 14 s, and `t/09-map` from a 120 s timeout to 35 s.
