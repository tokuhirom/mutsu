# Supply.merge keeps cold values through map/whenever; interval quits under CurrentThreadScheduler

Working the `EventSource::Server` distribution (all 5 test files now pass under mutsu):

- `Supply.merge($live, $cold)` seeds the cold values next to its supplier. `.map`/`.grep` on
  the merged supply and a `whenever` on it silently dropped those values; they are now
  transformed / delivered.
- `Supply.interval` under a `CurrentThreadScheduler` `$*SCHEDULER` now goes through the
  scheduler, so `.cue(:every)` refuses and the enclosing `supply` block quits (as Rakudo does)
  instead of hanging. A `whenever` on a scheduler-driven interval now cues it too.
- The `CurrentThreadScheduler` `:every` message matches Rakudo ("Cannot specify :every in
  CurrentThreadScheduler").

Known divergence: a supply block whose only `whenever` is the failing interval throws from
`tap` in Rakudo but routes to `quit` here.
