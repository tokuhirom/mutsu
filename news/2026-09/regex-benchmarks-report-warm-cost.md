# The regex benchmarks report warm cost, and YAMLish gets a rakudo baseline

ADR-0099 §6 listed four changes to the regex benchmark suite that were never made
([#9916](https://github.com/tokuhirom/mutsu/issues/9916)). All four are done now,
ahead of ADR-0135 Slice A, whose kill criterion needs them.

- **Warm cost.** Each of the eight regex and grammar files now runs its workload
  twice untimed, then prints a third, timed run as `bench-section-seconds:`.
  `scripts/bench-ci.sh` records that run as the file's `@section` series, and
  the series' rakudo column is rakudo's settled time. The whole-script rows keep
  charging rakudo its JIT warm-up, which is what made ADR-0099's first headline
  4.2x instead of 1.7x. Under `BENCH_DET=1` the workload runs once, so the
  instruction-count series is unchanged.
- **A real module grammar.** `bench-grammar-json-tiny` parses a 32 KB document
  with `JSON::Tiny::Grammar`, loaded from `modules/JSON-Tiny` by both
  interpreters.
- **A YAML baseline.** `bench-yaml-parse-big` loads YAMLish and MIME::Base64
  from `modules/` too, so its ratio column stops reading NA.
- **Past the crossover.** The long-subject scan no longer has a crossover to
  size past. The Stage 1 prefilter rejects most of its positions, and its warm
  section is 8 ms against rakudo's 204 ms. `bench-regex-scan-walk` takes over
  the job of exposing the walk's own per-position cost, with scans no prefilter
  can help. They are ADR-0135 §2.3's rows, at 160 KB.

Warm section, release, this container, 2026-09-30:

| benchmark | mutsu | rakudo |
|---|---:|---:|
| regex-match | 108 ms | 393 ms |
| regex-capture | 157 ms | 592 ms |
| regex-global | 51 ms | 121 ms |
| regex-assertion | 45 ms | 255 ms |
| regex-split-subst | 77 ms | 183 ms |
| regex-long-subject | 8 ms | 204 ms |
| grammar-parse-big | 27 ms | 106 ms |
| grammar-json-tiny | 42 ms | 127 ms |
| regex-scan-walk | 521 ms | 897 ms |
| yaml-parse-big | **179 ms** | **32 ms** |

The last row is the one the missing baseline had hidden. Warm, YAMLish on a
60-row document is 5.5x slower under mutsu than under rakudo. Every other row is
a mutsu win.
