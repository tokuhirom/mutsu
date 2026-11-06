# Sequence::Generator: `nqp::handle(.., 'LAST', ..)` and PredictiveIterator defaults

Two gaps stopped Sequence::Generator's `t/02-exhaustive`:

- `nqp::handle(body, 'LAST'|'NEXT'|'REDO', handler)` treated every category as `CATCH`, so a generator closure that called `last` (`(1, 2, { last if $_ >= 5; $_ + 1 })`) unwound through the sequence iterator into the caller's loop. The loop categories now take only that control exception (via a CONTROL region); `CATCH` is unchanged and errors pass a `'LAST'` handler.
- A class that composes only `PredictiveIterator` got none of the `Iterator` defaults (`skip-one`, `skip-at-least`, ...). The Iterator role prelude now also declares `role PredictiveIterator does Iterator`, as rakudo does.

Sequence::Generator t/01, t/02 and t/03 now match rakudo (rc 0, no `not ok`).
