# Battery survey: Data compression and archiving

**Slot:** stream compression (zlib/gzip, bzip2, lzma/xz, zstd) and archive
formats (zip, tar) · **Status:** **no candidate bundled — survey only**
(2026-08-22) · **Yardstick:**
[BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria) — license (hard
gate) → dependency weight → proven behaviour on mutsu → API fit ·
**Procedure:** [selection-method.md](selection-method.md)

Flagged as gap #4 in
[python-stdlib-comparison.md](python-stdlib-comparison.md)'s "Data
Compression and Archiving" section: Python's `zlib`/`gzip`/`bz2`/`lzma` and
`zipfile`/`tarfile` have no mutsu equivalent at all. This is the first pass
at the slot.

## A note on the no-native-dependency rule

The CSV survey ([csv.md](csv.md)) disqualified candidates with a native/C
dependency outright, because that slot had healthy pure-Raku alternatives.
That rule does **not** transfer here (user decision, 2026-08-22): a pure-Raku
compression codec is rare and slow, so essentially every credible candidate
in this field is a NativeCall binding to a standard system library
(`libz`, `libbz2`, `liblzma`, `libarchive`) — architecturally the same shape
as the already-bundled `OpenSSL` (binds system `libssl`/`libcrypto`) and
`DBIish` (binds system `libsqlite3`). All four of those system libraries are
present on this survey machine (`ldconfig -p`: `libz.so`, `liblzma.so`,
`libbz2.so`, `libarchive.so`, all versioned + unversioned symlinks). Native
dependency alone is therefore not a disqualifier in this slot; a *niche* or
*build-from-source* native dependency still is (see `Compress::Snappy`,
`Compress::Brotli` below).

## The field

Enumerated from `~/.zef/store/{rea,fez}/*.json` by filtering name/description
on `zip|gzip|gz|bzip2|bz2|lzma|xz|zlib|tar|archive|compress|deflate|inflate`.
`fez.json` (7,810 dists) carried a strict subset of what `rea.json` (14,834
dists) has for this slot — none of the codec bindings below are on fez at
all, only reachable through the REA mirror — so `rea.json` is the field of
record here, consistent with selection-method.md's own note that it is "the
more useful of the two." Perl 5 CPAN dists that leaked into the REA index by
name collision (`Bundle-Compress-Zlib`, `IO-Compress-Lzma`, etc. — tagged
`['perl_5']`) are excluded; they are not Raku modules. General-purpose tools
that merely touch archives as one feature among many (`Sparrowdo::Archive`,
`WebService::HazIP`) are excluded as not slot candidates.

**No Raku `lzma`/`xz` binding exists in the ecosystem at all** — the only hit
for that keyword is the Perl 5 `IO-Compress-Lzma` false-positive. `zstd` has
exactly one candidate. This means the slot's realistic ceiling today is
zlib/gzip + bzip2 + zip/tar via libarchive (which itself supports xz/lzma
*archive* filters internally, covering that gap indirectly for archives, just
not for standalone `.xz` streams).

### Compression codecs

| Candidate | Version | Released | License | Runtime deps | GitHub | Dependents¹ | raku | **mutsu** |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **`Compress::Zlib::Raw`** | 1.0.1 | 2018-04-26 | MIT² | none | [retupmoca/P6-Compress-Zlib-Raw](https://github.com/retupmoca/P6-Compress-Zlib-Raw) — ★3, last push 2024-03-22, not archived | 3 | **1/1** (7 tests) | **1/1** ✅ (7/7) |
| **`Compress::Zlib`** | 1.1.0 | 2019-03-11 | MIT² | `Compress::Zlib::Raw` | [retupmoca/P6-Compress-Zlib](https://github.com/retupmoca/P6-Compress-Zlib) — ★4, last push 2022-03-16, not archived | 9 | **3/3** (18 tests) | **0/3** ❌ — 2 distinct blockers, see below |
| **`Compress::Bzip2::Raw`** | 0.2.2 | 2021-03-31 | Artistic-2.0 | none | [Altai-man/perl6-Compress-Bzip2-Raw](https://github.com/Altai-man/perl6-Compress-Bzip2-Raw) — ★1, last push 2023-09-16, not archived | 1 | **1/1** (9 tests) | **1/1** ✅ (9/9) |
| **`Compress::Bzip2`** | 0.4.1 | 2021-03-31 | Artistic-2.0 | `Compress::Bzip2::Raw` | [Altai-man/perl6-Compress-Bzip2](https://github.com/Altai-man/perl6-Compress-Bzip2) — ★2, last push 2023-09-16, not archived | 3 | **1/1** (10 tests) | **0/1** ❌ — parse failure, see below |
| `Compress::Zstd` | 0.0.3 | 2019-09-12 | Artistic-2.0 | none | [timo/Compress-Zstd](https://github.com/timo/Compress-Zstd) — ★0, last push 2019-09-12, not archived | 0 | **2/2**³ (12 tests) | not measured⁴ |
| `Compress::Snappy` | 0.0.3 | — | MIT² | `NativeCall` | [avuserow/perl6-compress-snappy](https://github.com/avuserow/perl6-compress-snappy) — ★5, last push 2022-06-29, not archived | 0 | **0/3**⁵ | not measured⁵ |
| `Compress::Brotli` | 0.1.0 | 2017-05-14 | Artistic-2.0 (README only)⁶ | `NativeCall`, `LibraryMake` | [sylvarant/Compress-Brotli](https://github.com/sylvarant/Compress-Brotli) — ★0, last push 2017-05-14, not archived | 0 | **0/2**⁶ | not measured⁶ |

¹ Distributions in the local REA index whose `depends` names the candidate —
same method as [csv.md](csv.md)/[templates.md](templates.md).
² Declared `license: None` in `META6.json`, but a `LICENSE` file **is**
shipped and states MIT unambiguously — cross-checked and treated as MIT per
selection-method.md's "cross-checked against a shipped LICENSE" rule; this is
a META6.json omission, not an actual license gap (contrast with
`Compress::Brotli` below, which has no `LICENSE` file at all).
³ `01-basic.t` passes fully (6/6). `02-compressor.t` also completes fully
(6/6) but takes >30s wall-clock on its two "really-big" (8 MB) round-trip
cases — confirmed to pass at a 60s timeout. Functional, just slow to gate at
the standard 30s budget.
⁴ Not measured under mutsu — 0 dependents, and the raku suite's own slowness
means a from-scratch mutsu run risked eating the survey's time budget for a
marginal candidate. A future pass should measure it; nothing here suggests
it would behave differently from the other zlib/bzip2 codecs architecturally.
⁵ The system ships `libsnappy.so.1` (confirmed via `ldconfig -p`) but not the
unversioned `libsnappy.so` dev symlink NativeCall's `is native('snappy')`
resolution wants — `libsnappy-dev` is not installed on this survey machine.
This is an **environment gap, not a raku/mutsu bug**: `raku`'s own run fails
identically (`Cannot locate native library 'libsnappy.so'`) before reaching
any test logic, so the mutsu run was not attempted. Whether it would work
under mutsu once the dev package is installed is unknown.
⁶ No `LICENSE` file and `license: None` in `META6.json`; the README carries
an Artistic-2.0 badge and text, which is *weaker* evidence than a shipped
`LICENSE` file (same tier as `CSV::Parser` in [csv.md](csv.md), not a hard
gate but a real weakness). Separately, and more decisively: unlike every
other candidate here, `Compress::Brotli` is not a simple `is native('brotli')`
binding — it needs a `LibraryMake`-driven build step to compile its own
native shim against brotli's dev headers, which are not installed on this
machine, so `use Compress::Brotli` fails under raku itself
(`Could not find LibraryMake`) before reaching any Brotli-specific code. Not
measured under mutsu for the same reason as `Compress::Snappy` — dead on
raku, so a mutsu run would tell us nothing. Also the stalest candidate in the
field (last commit 2017-05-14, 9 years old).

### Archive formats (zip / tar)

| Candidate | Version | Released | License | Runtime deps | GitHub | Dependents¹ | raku | **mutsu** |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **`Archive::Libarchive::Raw`** | 0.1.5 | 2023-01-28 | Artistic-2.0 | `NativeCall` | [frithnanth/perl6-Archive-Libarchive-Raw](https://github.com/frithnanth/perl6-Archive-Libarchive-Raw) — ★4, **last push 2025-04-29**, not archived | 2 | **6/6** (119 tests)⁷ | **1/6** ❌ — one general NativeCall bug, see below |
| **`Archive::Libarchive`** | 0.0.17 | 2023-01-19 | Artistic-2.0 | `Archive::Libarchive::Raw`, `NativeHelpers::Blob` | [frithnanth/perl6-Archive-Libarchive](https://github.com/frithnanth/perl6-Archive-Libarchive) — ★5, **last push 2025-04-29**, not archived | 4 | **6/6** (45 tests)⁷ | **1/6** ❌ — inherits the Raw blocker |
| **`Archive::SimpleZip`** | 0.8.0 | 2023-09-16 | Artistic-2.0 | `Compress::Zlib`(::Raw), `IO::Glob`, `CompUnit::Util` | [pmqs/Archive-SimpleZip](https://github.com/pmqs/Archive-SimpleZip) — ★1, last push 2023-09-16, not archived | 0 | **3/3** (38 tests)⁷ | **0/1** ❌ — parse failure, see below |
| `LibZip` | * (2019) | 2019-09-29 | MIT | `NativeHelpers::Blob` | [azawawi/perl6-libzip](https://github.com/azawawi/perl6-libzip) — ★2, last push 2019-09-29, not archived | 0 | **1/1** (2 tests)⁸ | **0/1** ❌ — parse failure, see below |
| `IO-Archive` | 0.0.5 | 2024-06-22 | Artistic-2.0 (README only) | `Archive::Libarchive`, `Archive::Libarchive::Constants`, `MONKEY-TYPING` | [ssotka/IO-Archive](https://github.com/ssotka/IO-Archive) — ★1, last push 2024-06-22, not archived | 0 | **1/1** (1 test)⁹ | not measured⁹ |
| `Archive::Tar::PP` | v0.0.1 | 2021-05-19 | **none declared anywhere** | none | [tony-o/perl6-archive-tar-pp](https://github.com/tony-o/perl6-archive-tar-pp) — ★1, last push 2021-05-19, not archived | 0 | **4/4**¹⁰ (26 tests) | **1/4**¹⁰ — disqualified on license regardless |

¹ Same method as above.
⁷ Excludes the author-only `99-meta.rakutest`/`meta.t` files, which need the
unbundled `Test::META` module (dist-metadata QA, not functionality) — same
waiver as `Text::CSV`'s `99_meta.t` in [csv.md](csv.md).
⁸ The entire shipped test suite is two trivial assertions (`use` succeeds;
`.new` succeeds) — no round-trip coverage of actual zip read/write. A much
weaker depth signal than every other candidate in this table.
⁹ `IO-Archive`'s own suite is a single `use-ok` line — it is a thin
convenience wrapper adding `IO::Path` integration on top of
`Archive::Libarchive` (its hard dependency), so it inherits that dependency's
mutsu blocker below regardless. Not separately measured under mutsu given
the near-zero marginal information a `use-ok`-only file would add.
¹⁰ Pure Raku, zero runtime dependencies, and functionally the healthiest
archive candidate under raku short of `Archive::Libarchive` (`00-use.t`,
`01-pax.t`, `03-refuse.t`, `04-peek.t` all pass; `02-gittar.t` needs a `git`
binary in the test environment, not attempted; `05-write-peek.t` not
attempted). Included for completeness only — see "Ruled out" below, it is
disqualified on license before the mutsu number matters. The one file
measured under mutsu (`00-use.t`) passes; the others hit two more mutsu bugs
(`Index out of range` in tar-header buffer parsing, and an unsupported
`nqp::stat` op) — not filed as tickets since the module itself cannot be
bundled regardless of mutsu status, but noted here in case another
pure-Raku, tar-header-parsing module surfaces the same shapes later.

**Also found, not a slot candidate:** `IO::Path::AutoDecompress`
([lizmat/IO-Path-AutoDecompress](https://github.com/lizmat/IO-Path-AutoDecompress)
— ★1, last push 2026-04-23, the most recently active repo in this whole
survey) — shells out to the external `gunzip`/`bunzip2`/`7z` **binaries** via
`run()` rather than binding any library, and is read-only (decompression only,
no write/compress side at all). Not evaluated further: it is a convenience
shim over external tools, not a codec/archive library, and a `run()`-based
design means it inherits whatever gzip/bzip2/7z binaries happen to be on the
host `PATH` rather than the deterministic system libraries this slot is meant
to target.

## What blocks mutsu today

Every ergonomic (non-`::Raw`) candidate in both tables is blocked by a real,
reproducible mutsu bug — the low-level `::Raw`/`Bzip2::Raw` 1:1 C bindings
work perfectly, but the moment a higher-level Raku wrapper adds its own logic
on top, something breaks. Four distinct, independently-filed bugs:

1. **[`nativecall-local-sub-shadows-imported-same-name.md`](../../todo/tickets/nativecall-local-sub-shadows-imported-same-name.md)**
   — `Compress::Zlib.pm6` declares its own `sub compress(Blob $data, Int
   $level = 6)`, which should shadow the 4-arg `sub compress(...)` imported
   from `Compress::Zlib::Raw` (Raku's own-file-declaration-shadows-import
   rule). mutsu resolves the *imported* symbol instead, so every call
   fails with an arity mismatch (`NativeCall: 'compress' expects 4
   argument(s), got 1`). Blocks `Compress::Zlib` entirely (0/3 files).
2. **[`nativecall-sizeof-cstruct-repr-unsupported.md`](../../todo/tickets/nativecall-sizeof-cstruct-repr-unsupported.md)**
   — `nativesizeof()` on a `class ... is repr('CStruct')` reports the class
   as `P6opaque` instead of recognizing its `CStruct` repr. A second,
   independent `Compress::Zlib` blocker (its streaming API, `t/02-stream.t`/
   `t/03-wrap.t`) — fixing bug 1 alone will not unblock these two files.
3. **[`nativecall-cpointer-repr-typed-param-returns-whatever.md`](../../todo/tickets/nativecall-cpointer-repr-typed-param-returns-whatever.md)**
   — passing a `repr('CPointer')`-typed value (an opaque native handle) as
   an argument to a *second* native call makes that call return `Whatever`
   instead of running and returning its declared type. This is the single
   blocker for the entire `Archive::Libarchive`/`Archive::Libarchive::Raw`
   pair — everything past the trivial `use`/version-string tests fails this
   way (1/6 files vs raku's 6/6). **This is the highest-leverage fix in this
   survey**: `Archive::Libarchive` is otherwise the strongest candidate found
   (Artistic-2.0, actively maintained — last push 2025-04-29, more recent
   than any other candidate in either table — 4 dependents, and covers
   zip/tar/gzip/bzip2/xz uniformly through one library).
4. Two parser failures, each real but not yet root-caused to a minimal
   repro — **[`compress-bzip2-ternary-parse-after-dynamic-export.md`](../../todo/tickets/compress-bzip2-ternary-parse-after-dynamic-export.md)**
   (a `?? BAREWORD !! BAREWORD` ternary misparses as a bareword-swallows-`!!`
   listop call, in a file whose constants come from a dynamic `sub EXPORT`
   built via `MY::` package introspection rather than static `is export`
   tags — blocks `Compress::Bzip2`, 0/1) and
   **[`archive-simplezip-samewith-placeholder-slurpy-parse.md`](../../todo/tickets/archive-simplezip-samewith-placeholder-slurpy-parse.md)**
   (a `.map:` block combining a `$^a` placeholder, `samewith(...)`
   redispatch, and a forwarded `|c` slurpy capture, followed by `;
   ++ $count` — blocks `Archive::SimpleZip`, 0/1). A third, lower-priority
   parse bug — **[`libzip-nativecall-callback-signature-type-parse.md`](../../todo/tickets/libzip-nativecall-callback-signature-type-parse.md)**
   (an anonymous `&(Pointer, Pointer, int64, int32 --> int64)` callback
   signature type used as a NativeCall parameter type) — blocks `LibZip`,
   0/1, but `LibZip` has 0 dependents and the thinnest test suite in the
   field, so this is not a priority pick.

None of these four bugs are compression/archive-specific — they are general
NativeCall and parser gaps that happened to surface here, in the same spirit
as the CSV survey's shared heredoc-in-sub-body bug ([csv.md](csv.md)). Fixing
bug 3 in particular would likely also help any other NativeCall binding that
threads an opaque CPointer handle through a chain of calls (a very common
NativeCall idiom — file handles, DB connections, compiled-regex handles, …),
not just `Archive::Libarchive`.

## Ruled out before a full measurement

- **`Archive::Tar::PP`** (v0.0.1, `tony-o`) — **no license declared
  anywhere**: `META6.json`'s `license` field is absent, no `LICENSE`/
  `LICENCE` file is shipped, and the README makes no mention of licensing
  terms either. Per selection-method.md's hard gate, this disqualifies it
  outright — the same precedent that dropped `HTML::Template`/
  `Text::Template` from the template slot and `Text::CSV::LibCSV` from the
  CSV slot. A genuine pity: it is otherwise the most promising pure-Raku,
  zero-dependency tar candidate found, healthy under raku (4/4 files
  measured). If its author ever adds a license statement, it is worth a
  fresh look — but not before.
- **`Compress::Brotli`** (0.1.0, `sylvarant`) — weakest license evidence in
  the field (README-only, no `LICENSE` file) *and* fails to even load under
  `raku` on this machine (`Could not find LibraryMake` — it needs a
  build-time compile step against brotli's dev headers, not installed here),
  *and* the stalest repository found (last commit 2017). Any one of these
  would deprioritize it; together they rule it out without a mutsu
  measurement being useful.
- **`Compress::Snappy`** (0.0.3, `avuserow`) — blocked in this environment
  by a missing `libsnappy-dev` package (the runtime `.so.1` is present, the
  unversioned dev symlink NativeCall wants is not), so `raku` itself cannot
  load it here. Not a license or architecture problem — if `libsnappy-dev`
  were installed this candidate would likely be viable; re-survey if snappy
  support specifically becomes a priority.
- **`IO::Path::AutoDecompress`** (`lizmat`) — shells out to external
  `gunzip`/`bunzip2`/`7z` binaries via `run()` rather than binding a
  library, and is read-only. Out of scope for a codec/archive library slot;
  see the field notes above.
- **`IO-Archive`** (0.0.5, `ssotka`) — a one-file, `use-ok`-only convenience
  wrapper over `Archive::Libarchive`. Its own test suite gives no additional
  signal beyond its dependency's, and it would inherit that dependency's
  mutsu blocker regardless. Not a distinct option from `Archive::Libarchive`
  itself.
- **`LibZip`** (`azawawi`, MIT) — thinnest test suite in the field (2 trivial
  assertions, no real read/write round-trip coverage) and 0 dependents;
  `Archive::Libarchive`/`Archive::SimpleZip` are both better-tested zip
  options. Kept in the table because it did surface a distinct parser bug
  worth recording, not as a candidate to prioritize.

## Recommendation

**No candidate is ready to bundle today.** The pattern here is the "expect
the answer to be 'fix mutsu first'" case selection-method.md calls out as
normal: every credible codec/archive candidate is healthy under raku, and the
low-level `::Raw` C bindings (`Compress::Zlib::Raw`, `Compress::Bzip2::Raw`)
already pass 100% under mutsu — but every higher-level, ergonomic wrapper on
top of them hits a real mutsu bug before its own suite can run. This is not a
field-selection problem; it is an interpreter-readiness problem, and this
survey's real output is the four filed bugs above, not a winner.

If/when those are fixed, the shape of a future decision:

1. **For stream compression**, `Compress::Zlib` (zlib/gzip, 9 dependents,
   MIT) and `Compress::Bzip2` (bzip2, 3 dependents, Artistic-2.0) are both
   well-tested, actively-reasonable candidates once bugs 1+2 (Zlib) and bug 4
   (Bzip2) are fixed. There is no live `lzma`/`xz` candidate in the
   ecosystem at all — that part of the Python gap (`lzma`) cannot be closed
   by adopting an existing module; only `Archive::Libarchive`'s internal xz
   support (for *archives*, not raw `.xz` streams) touches it.
2. **For archive formats**, `Archive::Libarchive` is the strongest
   candidate by a wide margin: it is the only one of the two ecosystems
   (zip *and* tar, via libarchive's format/filter abstraction) rather than a
   zip-only or tar-only tool, it is Artistic-2.0, has the most dependents (4)
   and by far the most recent upstream activity (last push 2025-04-29 — no
   other candidate in either table was touched after 2024), and is blocked
   by exactly one bug (bug 3 above) rather than the two-or-more each codec
   candidate needs. `Archive::SimpleZip` is a credible zip-only fallback if
   `Archive::Libarchive` turns out to need more than bug 3 once actually
   attempted, but it is architecturally weaker (needs a 4-dist dependency
   chain: `Compress::Zlib` + `::Raw` + `IO::Glob` + `CompUnit::Util`, versus
   `Archive::Libarchive`'s 2-dist chain) and has its own separate parser
   blocker (bug 4b) to clear too.
3. Whoever next picks up NativeCall work should treat **bug 3
   (`nativecall-cpointer-repr-typed-param-returns-whatever.md`)** as the
   highest-leverage single fix in this survey — it is both the sole blocker
   for the strongest archive candidate and a general pattern (opaque
   CPointer handle threaded through a call chain) likely to recur in other
   NativeCall-based batteries, not just this slot.

Re-run this survey (or at least re-measure the four filed bugs) before acting
on any of the above — per selection-method.md, a readiness claim nobody just
re-measured is not evidence.
