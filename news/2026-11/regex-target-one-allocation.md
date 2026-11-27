# A regex match attempt allocates five times less for its setup

Every regex match attempt builds a `MatchTarget`, hit or miss: the subject's
`char` vector and the state the attempt's `Match` tree shares. On a short
subject this took six allocations, and the pattern text was copied twice more.

- **The `char` vector.** `text.chars().collect::<Arc<[char]>>()` regrew its
  intermediate `Vec` (`Chars` promises only a quarter of the byte length as a
  lower bound), then copied it into a fresh `Arc`. That was four allocations.
  An ASCII subject now collects its bytes straight into one exactly-sized
  `Arc<[char]>`. Any other subject counts its chars first, which takes two
  allocations and never regrows.
- **The shared run state.** The grammar cursor class and the lazily built
  mark-stripped view were two separate `Arc`s. They now live in one
  `RunState`. A subject cache entry still shares its stripped view across
  runs, via `StrippedSlot::Shared`.
- **The pattern text.** `regex_match_with_captures_value` and
  `parse_regex_value` each took a `to_string()` copy of the regex's pattern on
  every attempt. They now take a refcount bump of the `Arc<String>` the value
  already holds.

`scripts/bench-det.sh`, release build, against the previous regex change
(`3d0005e9`):

| benchmark | instructions | allocations |
|---|---|---|
| bench-regex-match | 307.0M → 278.6M (-9.3%) | 231.8k → 176.5k (-23.8%) |
| bench-regex-assertion | 193.2M → 176.1M (-8.9%) | 132.5k → 94.0k (-29.0%) |
| bench-regex-capture | 440.6M → 423.9M (-3.8%) | 412.2k → 377.3k (-8.5%) |
| bench-regex-split-subst | 410.6M → 395.7M (-3.6%) | 296.4k → 288.4k (-2.7%) |
