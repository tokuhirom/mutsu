# A `~~` on a short string builds its match target once, not twice

Every `$str ~~ /rx/` primed the subject cache through
`MatchTarget::primed_subject`, so that the engine's `MatchTarget::new` on the
same string could share one target. The cache keeps only subjects of 256
bytes or more, though. For a shorter subject, `of_subject` built a whole
target (the subject's `char` vector and three `Arc`s) that nothing kept, and
the engine then built its own. A typical log line, a word or a field paid for
two builds on every match.

`primed_subject` now primes only a subject the cache will keep. A short one
goes straight to the engine, which builds the one target it needs. This
removes ~2.8k instructions from every match on a short subject.

Two smaller per-attempt costs went with it:

- The engine's `MatchTarget::new(&str)` copied a short subject into a fresh
  `String` and `Arc`, although the `~~` that called it already held the
  subject's payload. `smart_match` now pins that payload for the duration of
  the match (`MatchTarget::pin_subject`), and `new` builds from it when the
  `&str` is exactly its bytes. That saves two allocations and a copy per
  match.
- `reset_capture_env_vars` copied the whole capture-shaped symbol registry
  (two `Vec`s, under a lock) on every match, only to find nothing to clear
  when no captures are in scope. It now collects only the keys the env
  holds.

`scripts/bench-det.sh` against `main` at `89c80cf5`:

| benchmark | instructions | allocations |
|---|---|---|
| bench-regex-match | 343.6M → 307.8M (-10.4%) | 318.6k → 232.9k (-26.9%) |
| bench-regex-assertion | 215.2M → 193.8M (-9.9%) | 192.4k → 133.6k (-30.5%) |
| bench-regex-capture | 460.2M → 441.5M (-4.0%) | 465.4k → 413.3k (-11.2%) |
