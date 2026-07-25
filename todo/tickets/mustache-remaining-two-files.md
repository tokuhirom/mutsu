# `Template::Mustache`: the last two upstream test files

The template battery passes 11 of its 13 upstream test files under mutsu,
including the whole official mustache spec suite (`91-specs`, 10/10). Two remain.
Both pass under raku. **Both have been root-caused far enough to start coding.**

## Reproducing

The dist is bundled, but the *tests* are not vendored — fetch them at the pinned
commit:

```sh
curl -sSL 'https://raw.githubusercontent.com/raku/REA/main/archive/T/Template%3A%3AMustache/Template%3A%3AMustache%3Aver%3C1.2.6%3E%3Aauth%3Czef%3Araku-community-modules%3E%3Aapi%3C1.2.0%3E.tar.gz' \
  | tar xz -C tmp/
cd tmp/Template::Mustache*/
mutsu -I lib t/06-logging.rakutest
mutsu -I lib t/92-specs-file.rakutest
raku  -I lib t/06-logging.rakutest      # the baseline: both pass
```

Run from the dist's own directory — the tests reach for `t/…` fixtures by
relative path.

---

## 1. `06-logging.rakutest` — 2/3 — **root cause found**

**It is `todo/tickets/scalar-attribute-subscript-assignment-lost.md`.** Fix that
and this file should follow.

The module's `Logger` (lib/Template/Mustache.rakumod:14-38) declares

```raku
class LoggersMap is Hash does Associative[Callable, LogLevel] { }
has LoggersMap $.routines;
submethod BUILD(LoggersMap :$!routines = LoggersMap.new, …) {
    for LogLevels.pairs { $!routines{.key} ||= … }
}
```

Under mutsu `$!routines` comes out **empty** — subscript assignment through a
`$`-sigil attribute is silently dropped. Observed directly:

```
raku:  routines-before: Audit,Debug,Error,Fatal,Info,Trace,Trace2,Verbose,Warn
mutsu: routines-before: (empty)
```

so the test's `$m.logger.routines<Warn> = &die` lands in a map nothing reads,
`render` does not throw, and `dies-ok` fails. mutsu prints
`Error while logging [Field not found ❮missing❯]:` instead.

---

## 2. `92-specs-file.rakutest` — 1/10 — **narrowed to `:from` path resolution**

Every `subtest` plans its N tests and runs **0** — the body dies before the first
assertion. The exception is:

```
X::IO::Resolve: /…/tmp/spec-partials/comments.json/1
```

i.e. `$m.render: 'specs-file-main', :$from, …` fails to resolve the *relative*
`:from` directory. The test builds it as
`views.basename.IO.add($subdir)` → `spec-partials/comments.json/1`, while the
files were written under `t/spec-partials/comments.json/1`, so raku and mutsu
evidently resolve that relative path against **different bases**. Compare how
each resolves a relative `IO::Path` inside the module's `get-template` /
`parse-template` (`lib/Template/Mustache.rakumod:331`, `:370`) — `$*CWD` vs the
script's directory is the first thing to check.

Already ruled out, each verified in isolation against raku:

- `load-specs` from the test's own `Template::Mustache::TestUtil`
- the anonymous state counter `++$` used to build a unique directory
- `.mkdir`, and the two-at-a-time
  `for 'name', $_<template>, |$_<partials>.kv -> $name, $text` loop
- the partial **cache** — the module holds it in `has %!cache` (a `%`-sigil
  attribute), so bug 1 above does not touch it

`91-specs.rakutest` renders the *same* spec corpus from **strings** and passes
10/10, which localises this squarely to the file/partial loading path.

---

## Status

Not a release blocker: the gate is a per-file baseline, so the 11 passing files
are pinned in `batteries-whitelist.txt` and a regression in them fails a release,
while these two stay tracked here. See `docs/batteries/templates.md`.
