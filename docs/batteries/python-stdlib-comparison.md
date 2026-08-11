# Gap analysis: Python's standard library vs. Raku core + mutsu batteries

**Purpose:** Python's standard library is the reference point people mean when they
say "batteries included." This document walks every module in Python 3.13's
library reference (`docs.python.org/3.13/library/`) and records what covers the
same ground in Raku — as a core language feature, as a module Rakudo/mutsu ships
by convention (`Test`, `NativeCall`), as one of mutsu's vendored
[bundle](../../BATTERIES.md), as a Raku ecosystem module not currently bundled, or
as a genuine gap. It exists to make "what's missing" answerable at a glance, and
to feed [PLAN.md §1 B1](../../PLAN.md) ("finalize the bundle list") with concrete
candidates instead of guesswork.

**How to read the Status column:**

| Status | Meaning |
| --- | --- |
| **Core language** | Built into Raku itself — no `use` needed, or a spec-mandated core routine/pragma. Often a *superset* of the Python module (Raku's numeric tower, regex/grammar engine, and object system are broader than the stdlib modules they replace). |
| **Core/bundled** | Ships with every Rakudo (and mutsu) by convention, via `use Name;`, even though it is technically "a module" (`Test`, `NativeCall`). |
| **mutsu battery** | Vendored under `modules/` and resolved with zero configuration — see the [bundle index](../../BATTERIES.md#7-bundle-index). |
| **Ecosystem (not bundled)** | A real Raku module exists on the zef/fez ecosystem, installable with `mzef install`, but mutsu does not vendor it — needs network access. Existence is based on general knowledge of the ecosystem as of this writing, not a fresh index query; confirm with `mzef search <name>` before relying on it for a decision. |
| **Gap** | No commonly-used Raku equivalent is known. |

This is a **snapshot**, not a live index — ecosystem module names can change or go
unmaintained, and mutsu's bundle grows over time (check
[BATTERIES.md §7](../../BATTERIES.md#7-bundle-index) for the current list before
trusting a "mutsu battery" row here).

Sections mostly follow Python's own library-reference grouping. A few
low-relevance sections for this project (Tk GUI, MS-Windows-specific services,
low-level Unix services, CPython-internal language services like `dis`/`ast`) are
condensed to a one-line note rather than a full table — they either don't map to
a general-purpose scripting battery, or Raku solves the underlying need
structurally (e.g. reflection via `.^methods`/`.HOW` instead of a `dis`/`ast`
module).

## Text Processing Services

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `string` | `Str` methods, core string routines | Core language | |
| `re` | native regex/grammar syntax (`/ /`, `token`, `rule`, `grammar`) | Core language | A superset — Raku's regex slang is a full mini-language with named captures, backtracking control, and grammars for structured parsing, not just a `re`-style function library. |
| `difflib` | — | Gap | `Algorithm::Diff` exists on the ecosystem but is not commonly relied upon; not bundled. |
| `textwrap` | `Str.indent`, manual line-wrap | Gap (partial) | No dedicated word-wrap module bundled. |
| `unicodedata` | `.uniprop`, `.NFC`/`.NFD`/`.NFKC`/`.NFKD`, `.uniname` | Core language | Deep native Unicode support (grapheme-aware strings by default) is a Raku design pillar, arguably ahead of Python here. |
| `stringprep` | — | Gap | Niche (SASL/email normalization); no known equivalent. |
| `readline` | — | Ecosystem (not bundled) | `Linenoise` / `Terminal::LineEditor` provide line editing; not vendored. |
| `rlcompleter` | — | Gap | Tied to `readline`. |

## Binary Data Services

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `struct` | `pack` / `unpack` | Core language | |
| `codecs` | `Encode`, `.encode`/`.decode` with builtin encodings | mutsu battery + Core | `Encode` is bundled ([http-deps.md](http-deps.md)); core `Str.encode`/`Blob.decode` already cover utf8/ascii/latin1 without it. |

## Data Types

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `datetime` | `DateTime`, `Date` | Core language | |
| `zoneinfo` | `DateTime` timezone offset | Core language (partial) | Core `DateTime` carries a UTC offset, but full IANA tzdata (DST rules, named zones) needs `DateTime::Timezones`-style tooling — not bundled. |
| `calendar` | `Date.is-leap-year`, `Date.day-of-week`, … | Core language (partial) | No text-calendar rendering; the date-math primitives are native. |
| `collections` | `Array`, `Hash`, `Set`, `Bag`, `Mix`, `List` | Core language | A superset — Set/Bag/Mix are first-class core types, not an extra import. |
| `collections.abc` | `Positional`, `Associative`, `Iterable` roles | Core language | |
| `heapq` | — | Gap | No bundled priority-queue module. |
| `bisect` | — | Gap | No bundled binary-search-insertion module. |
| `array` | typed `Array` (`array[int32]`) | Core language | |
| `weakref` | — | Gap | No standard weak-reference type in the Raku spec itself. |
| `types` | (types are core language constructs) | Core language | N/A — nothing to import. |
| `copy` | `.clone` | Core language | |
| `pprint` | `.gist`, `.raku`, `say` | Core language | Not a line-wrapping pretty-printer, but structural dumping is native. |
| `reprlib` | `.raku` | Core language | |
| `enum` | native `enum` declarator | Core language | |
| `graphlib` | — | Gap | No bundled topological-sort/graph module. |

## Numeric and Mathematical Modules

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `numbers` | Int/Rat/Num/Complex numeric tower | Core language | |
| `math` | core math routines (`sqrt`, `sin`, `log`, …) | Core language | |
| `cmath` | `Complex` methods | Core language | |
| `decimal` | `Rat` / `FatRat` | Core language | Exact rational arithmetic is the *default* numeric behavior for literals like `0.1`, arguably a stronger guarantee than opt-in `Decimal`. |
| `fractions` | `Rat` | Core language | |
| `random` | `rand`, `.pick`, `.roll` | Core language | |
| `statistics` | — | Ecosystem (not bundled) | `Statistics::Basic`-style modules exist; not vendored. |

## Functional Programming Modules

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `itertools` | lazy `Seq`, `.rotor`, `.combinations`, `.permutations`, `gather`/`take` | Core language | |
| `functools` | `.assuming` (currying), `reduce`, multi dispatch | Core language | |
| `operator` | operators are first-class subs (`&infix:<+>`) | Core language | |

## File and Directory Access

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `pathlib` / `os.path` | `IO::Path` | Core language | |
| `stat` | `IO::Path` methods (`.s`, `.modified`, `.mode`) | Core language | |
| `filecmp` | — | Gap | |
| `tempfile` | `File::Temp` | mutsu battery | [http-deps.md](http-deps.md) |
| `glob` / `fnmatch` | `dir()` + manual filtering | Core language (partial) | No confirmed bundled glob-pattern module; basic directory listing is native. |
| `linecache` | — | Gap | Niche. |
| `shutil` | `IO::Path` `.copy`/`.move`, `File::Directory::Tree` | Core + mutsu battery | Recursive tree operations lean on the bundled `File::Directory::Tree` ([http-deps.md](http-deps.md)). |

## Data Persistence

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `pickle` | — | Gap | No native object-serialization format; `.raku`/`EVAL` round-tripping is fragile and not a real substitute. Worth flagging since it's a commonly reached-for stdlib module. |
| `copyreg` | — | N/A | Tied to `pickle`. |
| `shelve` | `DBIish` + SQLite | mutsu battery (heavier substitute) | [database.md](database.md) — no lightweight persistent-dict equivalent, but a real embedded DB is bundled. |
| `marshal` | — | N/A | CPython-internal. |
| `dbm` | `DBIish` + SQLite | mutsu battery (heavier substitute) | Same as `shelve` — no simple single-file key-value store, but SQLite covers the use case. |
| `sqlite3` | `DBIish` (+ `NativeLibs`, `NativeHelpers::Blob`) | mutsu battery | [database.md](database.md) — a real SQLite DB can be opened/queried/written with zero install. |

## Data Compression and Archiving

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `zlib` / `gzip` / `bz2` / `lzma` | — | Gap | Not currently bundled; a real gap for anything that needs to read/write compressed streams. |
| `zipfile` / `tarfile` | — | Gap | mzef itself unpacks dist tarballs (Rust-side), but nothing is exposed to user Raku code. |

## File Formats

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `csv` | — | Ecosystem (not bundled) | Surveyed 2026-08-11: [csv.md](csv.md). `Text::CSV` (33/33 raku) and `CSV::Table` (10/10 raku) are both healthy under raku but currently blocked on mutsu by one shared, general compiler bug (`todo/tickets/heredoc-scope-check-false-positive-on-sub-body.md`); `CSV::Parser` (5/5) already works today as a thinner stopgap. |
| `configparser` | — | Ecosystem (not bundled) | INI-style config modules exist on the ecosystem; none bundled. |
| `tomllib` | — | Gap | No bundled TOML parser (mutsu's own `META6.json` handling is JSON, not TOML, so this hasn't been needed internally). |
| `netrc` / `plistlib` | — | Gap | Niche. |

## Cryptographic Services

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `hashlib` | `Digest` | mutsu battery | [digest.md](digest.md) — MD5, SHA-1, all SHA-2/SHA-3 widths + SHAKE, RIPEMD-160. No BLAKE2 (Python's `hashlib` has `blake2b`/`blake2s`). |
| `hmac` | `Digest::HMAC` | mutsu battery | [digest-hmac.md](digest-hmac.md) |
| `secrets` | `Crypt::Random` | mutsu battery | [crypt-random.md](crypt-random.md) — OS-entropy CSPRNG bytes; no `token_hex`/`compare_digest`-style convenience wrappers bundled on top. |

## Generic Operating System Services

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `os` | `%*ENV`, `run`, `shell`, `IO::Path` | Core language | Spread across several core facilities rather than one module. |
| `io` | core IO handles | Core language | |
| `time` | `now`, `DateTime` | Core language | |
| `logging` | — | Ecosystem (not bundled) | No general-purpose structured-logging framework bundled; `Log::Timeline` and similar exist on the ecosystem. |
| `platform` | `$*DISTRO`, `$*KERNEL`, `$*VM` | Core language | |
| `errno` | typed `X::*` exceptions | Core language | Different paradigm (typed exceptions vs. numeric codes) — arguably a superset. |
| `ctypes` | `NativeCall` | Core/bundled | Ships by convention (`use NativeCall`); a justified rung-3 native provider per `BATTERIES.md` §1, far more ergonomic than `ctypes`. |

## Command-Line Interface Libraries

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `argparse` / `optparse` | `sub MAIN(...)` with signatures | Core language | Argument parsing is generated from the `MAIN` sub's signature — arguably more idiomatic than either Python module. |
| `getpass` | — | Gap | No bundled no-echo password prompt. |
| `fileinput` | — | Gap | |
| `curses` | — | Ecosystem (not bundled) | NativeCall bindings to ncurses exist on the ecosystem; not vendored. |
| `cmd` | — | Gap | No bundled interactive-shell-builder. |

## Concurrent Execution

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `threading` | `Thread`, `Lock`, `Promise` | Core language | |
| `multiprocessing` | `Proc::Async` | Core language (different model) | Raku favors subprocess-based concurrency over `fork`-based shared-memory workers. |
| `concurrent.futures` | `Promise` | Core language | |
| `subprocess` | `run`, `shell`, `Proc::Async` | Core language | |
| `sched` | `Supply.interval` | Core language (partial) | Covers periodic-task scheduling; not a general job scheduler. |
| `queue` | `Channel` | Core language | |
| `contextvars` | dynamic variables (`$*VAR`) | Core language | A core language feature, not a bolted-on module. |

## Networking and Interprocess Communication

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `asyncio` | `Promise` / `Supply` / `react`/`whenever` | Core language | A different (reactive) native async model, not a direct port. |
| `socket` | `IO::Socket::INET`, `IO::Socket::Async` | Core language | |
| `ssl` | `OpenSSL` + `IO::Socket::SSL` | mutsu battery | [tls-openssl.md](tls-openssl.md) — real `https://` works end-to-end; binds the system `libssl` so CVEs ride the OS. |
| `select` / `selectors` | `Supply`/`react` reactive model | Core language | Different paradigm; no direct `select(2)`-style API needed. |
| `signal` | `signal()`, `Supply` | Core language | |
| `mmap` | — | Gap | No bundled wrapper; would need a raw `NativeCall` binding. |

## Internet Data Handling

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `email` | — | Ecosystem (not bundled) | `Email::MIME`-style modules exist; not vendored. |
| `json` | native `to-json` / `from-json` | mutsu native | Not spec-mandated in real Raku (`JSON::Fast` is a separate, if near-universal, module there) — mutsu provides it as a genuine built-in. See [BATTERIES.md §7](../../BATTERIES.md#7-bundle-index). |
| `mailbox` | — | Gap | |
| `mimetypes` | `MIME::Types` (transitively, via the HTTP/web stack) | Ecosystem (not bundled standalone) | |
| `base64` | `MIME::Base64`, `Base64` | mutsu battery | [base64.md](base64.md) — two distinct modules for the MIME-flavored and URI-safe alphabets. |
| `binascii` | `pack`/`unpack`, `Base64` | Core + mutsu battery | |
| `quopri` | — | Gap | Niche. |

## Structured Markup Processing Tools

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `html` / `html.parser` | — | Ecosystem (not bundled) | `HTML::Parser`-style modules exist on the ecosystem; not vendored. |
| `xml.etree.ElementTree` / `xml.dom` / `xml.sax` | — | Ecosystem (not bundled) | The community `XML` module (raku-community-modules) is the usual choice; not vendored. A notable gap given how common XML parsing is. |

## Internet Protocols and Support

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `webbrowser` | — | Gap | Niche. |
| `wsgiref` / `socketserver` / `http.server` | `Cro::HTTP` | Selected, not yet bundled | The web-framework slot — see [web-framework.md](web-framework.md) and [PLAN.md §1 B1](../../PLAN.md). This is the single biggest structural gap against the "small web blog with the bundle alone" yardstick. |
| `urllib.*` / `http.client` | `HTTP::UserAgent`, `URI` | mutsu battery | [http-client.md](http-client.md), [http-deps.md](http-deps.md) |
| `ftplib` / `poplib` / `imaplib` / `smtplib` | — | Gap | No bundled mail/FTP client protocols. |
| `uuid` | — | Ecosystem (not bundled) | A `UUID` module exists on the ecosystem; not vendored. |
| `http.cookies` / `http.cookiejar` | `HTTP::UserAgent`'s own cookie jar | mutsu battery | Covered as part of the HTTP client, not a standalone module. |
| `xmlrpc.*` | — | Gap | |
| `ipaddress` | — | Gap | No bundled IP-address parsing/manipulation module. |

## Multimedia Services

Low relevance to a general-purpose scripting bundle. `wave` and `colorsys` have
no bundled Raku equivalent (**Gap**) — not prioritized.

## Internationalization

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `gettext` | — | Gap | No bundled gettext-style i18n/translation catalog support. |
| `locale` | — | Gap | No bundled locale-aware number/currency/date formatting; formatting is manual today. |

## Development Tools

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `typing` | native type constraints, `where` clauses, subsets | Core language | Types are load-bearing in the language itself, not an opt-in annotation layer. |
| `pydoc` | `Pod::To::Text` / `Pod::To::HTML` (real Rakudo modules) | mutsu battery | [pod-to-text.md](pod-to-text.md) — retired from a native reimplementation to the genuine vendored module, per `BATTERIES.md`'s rung-3-retirement precedent. |
| `doctest` | — | Gap | No auto-testing of Pod6 code examples. |
| `unittest` | `Test` | Core/bundled | Ships by convention (`use Test;`), TAP-based rather than xUnit-style. |
| `unittest.mock` | — | Gap | No bundled mocking framework. |

## Debugging and Profiling

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `pdb` | — | Gap | No interactive debugger exposed to user Raku code (mutsu's own debugging tooling — `--dump-ast`, `MUTSU_TRACE` — is a developer tool for mutsu itself, not a language-level facility). |
| `faulthandler` / `profile` / `timeit` / `trace` / `tracemalloc` | — | Gap | No bundled profiling/tracing tooling; ad hoc timing via `now` is possible but unstructured. |

## Software Packaging and Distribution

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `ensurepip` / `venv` / `zipapp` | `mzef` (vendored Zef) | mutsu native | mutsu ships its own package manager binary — the direct analogue of `pip` (and, via per-project module paths, much of what `venv` gets used for). See `CLAUDE.md`'s "mzef package manager and distribution" section. |

## Python Runtime Services

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `sys` | `%*ENV`, `@*ARGS`, `exit()` | Core language | |
| `warnings` | `warn()` | Core language | |
| `dataclasses` | native `class` with attributes | Core language | Raku classes are concise enough that a separate "dataclass" layer isn't needed. |
| `contextlib` | `LEAVE`/`KEEP`/`UNDO` phasers | Core language | A different (phaser-based) mechanism for the same "run this on scope exit" need. |
| `abc` | `role` with stub methods (`{...}`) | Core language | |
| `atexit` | `END` phaser | Core language | |
| `traceback` | `.backtrace` on exceptions | Core language | |
| `gc` | (Bacon-Rajan cycle collector, ADR-0001) | Core (partial) | No user-facing GC control API; the collector itself is internal. |

## Importing Modules

| Python | Raku / mutsu | Status | Notes |
| --- | --- | --- | --- |
| `importlib` / `pkgutil` / `zipimport` / `modulefinder` | `use` / `require` | Core language | Raku's module system is a core language mechanism, not a separate importable layer. |

## Python Language Services, Tk GUI, MS-Windows- and Unix-specific services

Out of scope for a direct comparison: `ast`/`dis`/`tokenize`/`symtable` map onto
Raku's native meta-object protocol (`.^methods`, `.HOW`, `.raku`) rather than a
bytecode-inspection module; `tkinter` has no bundled GUI toolkit equivalent
(**Gap** — a real one, but low priority for a server/scripting-focused bundle);
low-level Unix/Windows modules (`termios`, `fcntl`, `winreg`, …) are one-off
`NativeCall` binding territory rather than stdlib-shaped modules.

## Summary — the highest-value gaps

Ranked by how often the underlying need shows up in ordinary scripts, the gaps
most worth a battery survey next (methodology:
[selection-method.md](selection-method.md)):

1. **A web framework** (`Cro::HTTP`) — already surveyed and selected, not yet
   bundled; the single biggest hole against the project's own "small web blog"
   yardstick. See [web-framework.md](web-framework.md).
2. **CSV** (`Text::CSV` / `CSV::Table` / `CSV::Parser`) — extremely common in
   scripting; surveyed 2026-08-11, see [csv.md](csv.md). Blocked on a general
   compiler bug rather than a weak field — the two strongest candidates are
   healthy under raku.
3. **XML parsing** (`XML`) — common for config/data interchange; no survey yet.
4. **Compression/archiving** (`zlib`/`gzip`/`tarfile`/`zipfile` equivalents) — no
   bundled story at all today.
5. **A logging framework** — `print`/`say`-based debugging works, but nothing
   structured is bundled.
6. **UUID generation** — small, self-contained, commonly needed by web/DB code
   that's otherwise already covered (`DBIish`, `HTTP::UserAgent`).
7. **`configparser`/INI or TOML config parsing** — pairs naturally with the
   logging and web-framework gaps for "write a small service" use cases.

Everything else — `pickle`, `shelve`/`dbm`, `heapq`/`bisect`, `weakref`,
`gettext`/`locale`, `pdb`/profiling tools — is either a genuine language-level
absence (no equivalent primitive exists in Raku, not just "no module yet") or
low-value enough that no survey is warranted without a concrete driving use
case.
