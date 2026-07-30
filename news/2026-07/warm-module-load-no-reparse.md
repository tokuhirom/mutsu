# Warm module load no longer re-reads or re-parses module sources

`news/2026-07/module-export-scan-cache.md` left a residual: a fully warm
(precompiled) `use Template::HAML` still ran 0.29s against raku's 0.21s, with
strace showing 105 `.rakumod` opens for 30 modules. The parser's export scan
was already down to one open per file; the rest was the runtime load path
doing per-module work that a warm load does not need:

- `load_module` read the module source and ran a full `parse_program_partial`
  over it — even on a precomp hit — just to extract exported operator names
  (`infix:<..>` etc.) for EVAL visibility. The names are now extracted by
  walking the statements `parse_module_source` already returns, which are
  identical for a fresh parse and a cache hit, so the extra read and the whole
  parse are gone.
- Precomp validation re-read the source to compute the content hash, a few
  lines after `parse_module_source` had read the same bytes into memory.
  `load_cached_unit` now accepts the already-read source and hashes it
  in-memory; the hash function is unchanged, so existing cache entries stay
  valid. (The `require` path still passes `None` and reads from disk — it has
  no source in scope on a hit.)
- `interpreter_version()` stat'ed the interpreter binary (`current_exe` +
  metadata) on every validation to build the version stamp; it is now memoized
  in a `OnceLock` since the exe mtime cannot change mid-process.

## Results (release build, 2026-07-30, Template::HAML dist)

| | before | after | raku |
| --- | --- | --- | --- |
| `use Template::HAML` (warm precomp) | 0.29s | **0.21s** | 0.21s |
| `.rakumod` file opens per load | 105 | **53** | — |

Warm module load now matches raku. The remaining ~53 opens are one parse-time
export-scan read plus one runtime read per module, both fundamental to the
current design (the runtime read feeds the `no precompilation` directive scan
and the content-hash validation).

Pinned by `precomp::tests::in_memory_source_validates_like_disk_read`; the
EVAL-sees-imported-operators behavior stays pinned by
`t/import-operator-method.t`, and cold/warm operator visibility was verified
by hand against raku. Investigating this also surfaced a pre-existing
EVAL-closure import bug, recorded in
`todo/tickets/eval-closure-loses-imported-subs-after-scope-pop.md`.
