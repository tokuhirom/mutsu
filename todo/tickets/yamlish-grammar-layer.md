# YAMLish fails in the YAML grammar itself (the layer after the dispatch blocker)

Split out of `todo/tickets/yamlish-grammar-parse-dispatch.md` on 2026-07-26.
That ticket's blocker is gone — the module loads, `Grammar.parse(...)` dispatches
correctly, and the `.HOW` residue it also recorded is fixed
(`news/2026-07/grammar-how-metaclass.md`). `YAMLish` now fails further in.

## Measured 2026-07-26 (YAMLish 0.1.3 from the REA archive, `mutsu -I lib`)

| file | ok | not ok |
| --- | --- | --- |
| `t/anchor-alias.rakutest` | 0 | 2 |
| `t/basic.rakutest` | 0 | 7 |
| `t/p5-tests.rakutest` | 1 | 8 |
| `t/roundtrip.rakutest` | 9 | 13 |
| `t/test-harness.rakutest` | 16 | 25 |

`test-harness` reports `Couldn't parse YAML: Couldn't parse YAML`, i.e. the
grammar's own failure path, so the parse is reaching real input and rejecting it
rather than dying on a dispatch or load error.

Get the dist with:

```
curl -sS https://raw.githubusercontent.com/raku/REA/main/META.json \
  | python3 -c "import sys,json;[print(x['source-url']) for x in json.load(sys.stdin) if x.get('name')=='YAMLish' and x.get('version')=='0.1.3']"
```

## Why it is not a ticket-sized fix yet

The YAML grammar is `lib/YAMLish.rakumod:150–783` — large, action-heavy, and
almost certainly failing on several independent constructs rather than one. It
needs the same treatment that worked for `Template::Mojo`: reduce the *real*
grammar by deleting constructs until a small repro falls out, one failing shape
at a time, rather than theorising from the first error line. Start with
`basic.rakutest` (7 failures, no anchors/aliases involved) since it exercises
the simplest documents.

## Context

`YAMLish` is the battery candidate for YAML (`docs/batteries/yaml.md`). Blockers
#1 and #1.5 were fixed earlier; #2 (this file's parent) is now fixed too.
