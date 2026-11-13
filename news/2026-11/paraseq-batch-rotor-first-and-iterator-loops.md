# ParaSeq: `.batch`/`.rotor` no longer hang, `.first(:k)` stays lazy, cheaper iterator loops

Running ParaSeq's whole suite next to rakudo turned up four problems besides the speed work of
ADR-17337:

- **`nqp::slice` of a list or an `IterationBuffer`.** It only knew Buf/Blob, so ParaSeq's
  `.batch` and `.rotor`, which slice their `IterationBuffer` batches, died inside every cued task.
  It now slices elements with MoarVM's semantics (a negative index counts from the end, an empty
  or out-of-range slice is `MVMArray: Slice index out of bounds`), into a store of the same kind.
- **A cued block that dies.** With no `:catch` and no `uncaught_handler`, rakudo reports
  "Unhandled exception in code scheduled on thread N" and exits 1. mutsu swallowed it, so the
  task's consumer waited forever: that is how the `nqp::slice` failure turned into two hung test
  files.
- **`.first` with `:k`/`:kv`/`:p`.** The pull-one-at-a-time `.first` over a `Seq.new($iterator)`,
  an unread `.map`/`.grep` Seq or a `gather` handled only the plain form (and `:k` on a gather);
  the adverb forms fell back to the eager scan and reified the whole sequence. ParaSeq's
  `first($matcher, :k)` at degree 3 processed all 200,000 elements.
- **Iterator loops.** Every method call on a built-in iterator classified it by string-keyed
  attribute probes, every `nqp::push`/`shift` on a `ConcBlockingQueue` found its channel by name,
  and the `IterationEnd` bareword went through the whole resolution chain on every read. They read
  pre-interned symbols now, and the `IterationEnd` site is remembered like a type object. A user
  iterator's `pull-one` loop dropped from 2.41G to 1.91G instructions for 42,000 pulls.

ParaSeq `t/27-batch` and `t/28-rotor` pass again (all 16 and 24 tests).
