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

`scripts/bench-det.sh` against `main` at `89c80cf5`:

| benchmark | instructions | allocations |
|---|---|---|
| bench-regex-match | 343.6M → 312.0M (-9.2%) | 318.6k → 261.7k (-17.9%) |
| bench-regex-assertion | 215.2M → 196.9M (-8.5%) | 192.4k → 153.4k (-20.2%) |
| bench-regex-capture | 460.2M → 444.2M (-3.5%) | 465.4k → 430.1k (-7.6%) |
