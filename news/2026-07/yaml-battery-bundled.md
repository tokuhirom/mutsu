# YAML is a bundled battery: `YAMLish` ships with mutsu

`YAMLish` (`zef:leont`, v0.1.3, Artistic-2.0) is vendored at `modules/YAMLish/`
and resolves with **zero config** — `use YAMLish` works from a plain `mutsu`
with no `-I` and no install:

```raku
use YAMLish;
my %conf = load-yaml("name: mutsu\ntags: [raku, yaml]\n");
say %conf<tags>[0];              # raku
say save-yaml(%conf);
```

It fills the YAML slot chosen in the 2026-07-25 survey
([docs/batteries/yaml.md](../../docs/batteries/yaml.md)): the ecosystem's
de-facto YAML module (459 dependents, three orders of magnitude ahead of the next
candidate), pure Raku, parser *and* emitter, and **safe by default** — tag
resolution runs through a fixed callback table with no `EVAL`, no arbitrary class
construction and no attacker-controlled `require`, so `load-yaml` *is* the
safe loader and there is no unsafe sibling to reach for by mistake. Its only
runtime dependency, `MIME::Base64`, was already bundled, so the bundle grows by
one file and no new dependency tree.

## What shipped

- `modules/YAMLish/` — `lib/` plus `META6.json`, `LICENSE`, `README.md`,
  `Changes`, verbatim from upstream tag `0.1.3` (commit `2a1d04ab`), verified
  byte-identical against a fresh clone. Upstream `t/`, `xt/`, `test-suite/`,
  `dist.ini` and `TODO.md` are excluded per BATTERIES.md §3.
- A `batteries.lock` row, so the release-time gate fetches the upstream suite at
  the pinned commit and runs it against the *bundled* copy; **all five files are
  whitelisted**, so a regression in any of them blocks a release.
- `t/yaml-battery.t` — the zero-config smoke test (block/flow collections, block
  scalars, anchors, multi-document, emit, round-trip).
- The battery record, the BATTERIES.md §7 index row, and a row on the site's
  Batteries page (generated from `META6.json` by
  `scripts/gen-batteries-manifest.py`).

## Correctness

All 5 upstream test files pass — **81 of 81 subtests, the same score `raku`
gets**. Getting there took the interpreter fixes written up in
`yamlish-upstream-suite-passes.md` and `yamlish-block-collections.md`; the
vendored source was never edited, per the "fix mutsu, not the module" rule.

Known gap: parsing is 20–40× slower than raku on the larger documents. That is
correctness-complete but throughput-poor, and is tracked separately in
`todo/tickets/yaml-parse-throughput.md`.
