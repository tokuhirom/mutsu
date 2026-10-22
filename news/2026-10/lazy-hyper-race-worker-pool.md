# A lazy `.hyper` / `.race` pipeline runs its stages on the worker pool

A `.hyper` / `.race` over an unbounded source used to stream its `map`/`grep`
stages serially (#14284), so `:degree` and `:batch` had no effect. A pull now
takes `degree x batch` source elements on the calling thread (64 x cores by
default, never unbounded), runs the stage over them on the worker pool with
`degree` batches in flight, and appends the results in source order. A settled
`.hyper` / `.race` honours a user `:batch` / `:degree` too (it ignored them and
used one worker per item).

Release build, 4 cores, `(1..*).map(*+0).hyper(:batch(1), :degree(4)).map({ sleep 0.1; $_ }).head(8)`:
0.808 s before, 0.209 s after (settled, same pipeline: 0.104 s before the
settled path honoured `:degree(4)`, 0.203 s after).
