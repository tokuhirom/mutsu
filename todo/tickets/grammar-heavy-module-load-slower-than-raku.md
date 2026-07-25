# Loading a grammar-heavy module is 2–3× slower than raku

mutsu's headline is fast startup (~0.04× raku). Loading `Template::HAML` inverts
that: mutsu is **slower than raku**, and the cost is fixed per process rather
than proportional to the work done.

## Measurement (2026-07-25, release build)

```raku
use Template::HAML;
say "loaded";
```

| | raku | mutsu (release) |
| --- | --- | --- |
| `use Template::HAML` only | 0.345s | **0.794s** |
| `t/0030-tags.rakutest` | 0.286s | 0.869s |
| `t/0040-haml-render.rakutest` | 0.562s | 2.733s |
| 12 files, `t/0080`–`t/0190` | 0.31–0.94s | **pinned at ~1.6s** |

The "pinned at ~1.6s" row is the tell: whatever each of those files asserts,
mutsu pays about the same, so the dominant term is **compiling/loading the
module**, not executing the tests. `Template::HAML` is grammar-heavy — a large
grammar plus actions — which is the obvious suspect.

Reproduce with the dist from the REA archive:

```sh
curl -sSL 'https://raw.githubusercontent.com/raku/REA/main/archive/T/Template%3A%3AHAML/Template%3A%3AHAML%3Aver%3C0.9.5%3E%3Aauth%3Czef%3Agdonald%3E%3Aapi%3C1%3E.tar.gz' | tar xz
cd Template::HAML*/ && time mutsu -I lib -e 'use Template::HAML; say "loaded"'
```

## Measure release, not debug

A debug build shows ~20× on the same files. That is debug overhead and **not**
the real figure — it is what made this look like a hang at first. Always
characterise this one with `target/release/mutsu`.

## Why it is worth a ticket

Startup and load time are the properties mutsu sells, and a bundled battery is
loaded on every run of a program that uses it. A 2–3× regression against raku on
grammar-heavy module load is the kind of thing that shows up as "mutsu feels
slow" once the bundle grows. `MUTSU_VM_STATS` counters and a `--profile
profiling` build with `perf` are the tools; see the perf notes in `CLAUDE.md`.

Found during the template-battery survey — `docs/batteries/templates.md`,
`todo/deep/template-engines-blocked-on-mutsu.md`.
