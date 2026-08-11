# Battery survey: CSV

**Slot:** CSV read/write · **Status:** surveyed, **no candidate bundled yet** ·
**Yardstick:** [BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria) —
license (hard gate) → dependency weight → proven behaviour on mutsu → API fit
· **Procedure:** [selection-method.md](selection-method.md)

Flagged as gap #2 in
[python-stdlib-comparison.md](python-stdlib-comparison.md) — Python's `csv`
is a stdlib module with no direct Raku core equivalent, and CSV handling is
one of the most common "small scripting task" needs. This is the first pass
at the slot: enumerate the field from the local REA/fez indices, measure
license + dependents + raku baseline + mutsu result for each candidate, and
record what would have to happen before any of them can be bundled.

**Headline finding: nothing in the field is bundle-ready today, but not for
the reason usually seen in these surveys.** The two credible RFC4180-style
candidates are *healthy under raku* (Text::CSV 33/33 files, CSV::Table
10/10). What blocks them on mutsu is **one shared, general, false-positive
compiler bug** (`todo/tickets/heredoc-scope-check-false-positive-on-sub-body.md`,
filed by this survey) that misfires on the ordinary pattern "`my $x = ...;`
followed later by a `qq:to/…/` heredoc referencing `$x`, inside the same
`sub`" — nothing CSV-specific about it. Fixing that one compiler bug is
likely the highest-leverage next step for this slot, not picking a winner.

## The field

Enumerated from `~/.zef/store/{rea,fez}/*.json` (the same indices `mzef`
uses) by filtering name/description/tags on `csv`. `App::Rak` and
`Data::Reshapers`/`Data::Summarizers` matched the keyword search but are
general-purpose tools (a grep-alike, a data-reshaping toolkit) that merely
support CSV as one input format among several — excluded as not CSV-slot
candidates.

| Candidate | Version | Released | License | Runtime deps | Dependents¹ | raku | **mutsu** |
| --- | --- | --- | --- | --- | --- | --- | --- |
| **`Text::CSV`** | 0.022 | 2023-10-30 | Artistic-2.0 | `Slang::Tuxic`, `File::Temp` | 0 | **33/33** (22697 tests) | **0/33** — blocked at `use` |
| **`CSV::Table`** | 0.0.2 | 2025-05-31 | Artistic-2.0 | `YAMLish`, `JSON::Fast`, `File::Temp`, `Text::Utils` | 0 | **10/10** (184 tests) | **0/10** — blocked at `use` |
| **`CSV::Parser`** | 0.1.4 | 2023-06-06 | *(README only — see below)* | **0** | 0 | **5/5** | **5/5** ✅ |
| `CSV-AutoClass` | 0.2.0 | 2023-11-19 | Artistic-2.0 | `CSV::Parser`, `File::Find`, `Text::Utils` | 0 | 0/2 (missing test fixture on this checkout) | 0/2 — blocked at `use` |
| `Duck::CSV` | 0.0.2 | 2026-05-30 | MIT | `Duckie` (→ system `libduckdb`) | 0 | not measured² | not measured² |
| `Text::CSV::LibCSV` | 0.0.3 | 2022-09-09 | **none declared anywhere** | native `libcsv` (build-time C compile) | 0 | 0/5 — missing test fixtures | not measured³ |
| `JSON-CSV` | 0.0.1 | 2022-07-11 | Artistic-2.0 | `Text::CSV`, `JSON::Fast`, `JSON::Stream` | 0 | not measured | not measured |

¹ Distributions in the local REA index whose `depends` names the candidate —
same method as [templates.md](templates.md). **Zero for every candidate in
this field** — a materially weaker ecosystem-standing signal than the
template slot (`Template::Mustache` had 11) or the HTTP-client slot. CSV
handling in the Raku ecosystem is mostly done ad hoc, not through a shared
library.
² `Duckie` (the DuckDB NativeCall binding `Duck::CSV` wraps) was not
installed for this survey — see "Ruled out" below.
³ Ruled out on the license gate before a mutsu run was worth doing.

### Reading the raku column for `CSV-AutoClass` and `Text::CSV::LibCSV`

Both suites reference test data files (a CSV fixture, a `bin/` script) that
were not present in the REA archive tarball fetched for this survey — a
packaging gap in the *upstream dist*, not a raku behavior problem. Neither
was worth chasing further given the license/dependency issues below already
rule them out; noted so a future re-survey doesn't mistake it for a raku
regression.

## What blocks mutsu today

### `Text::CSV` and `CSV::Table` — one shared compiler bug

```
$ mutsu -e 'sub foo() { my $x = "hi"; print qq:to/HERE/;
value: $x
HERE
}
foo();'
Variable '$x' is not declared. Perhaps you forgot a 'sub' if this was intended to be part of a signature?
```

Neither module gets past `use` on mutsu. `Text::CSV.rakumod` hits it via its
own `my $opt`-style locals interpolated into `qq:to/` diagnostics;
`CSV::Table`'s dependency `Text::Utils::Subs.rakumod:157` hits the identical
shape (`my $opt-used = …; … qq:to/HERE/ … {$opt-used} … HERE`). Bisected down
to: **any `sub` body that declares a `my` local and later references it from
a heredoc gets a false "not declared" compile error** — confirmed independent
of hyphens in the name, ternaries, or heredoc indentation; a top-level
(non-sub) heredoc referencing a top-level `my` works fine, which is why this
had not surfaced before.

Root cause (see the filed ticket for the full trace): `check_heredoc_scope_errors`
(`src/compiler/helpers_block_inline.rs:477`) is meant to catch a real, narrower
gotcha — a heredoc that is physically outside the block that declared a
variable it references. But `compile_sub_body_with_deprecation`
(`src/compiler/helpers_sub_body.rs:196`) invokes it on the **sub's own
top-level body**, where "the declaring scope" and "the heredoc's enclosing
scope" are the same statement list — there is no leak to catch there, so it
false-positives on an extremely ordinary pattern. Filed as
`todo/tickets/heredoc-scope-check-false-positive-on-sub-body.md`.

This is worth fixing independent of the CSV slot decision: it is a general
false positive, not a CSV-specific gap, and it is the kind of "one interpreter
bug blocks N ecosystem modules" lever the template survey found with
`Template::Mustache` (see [templates.md](templates.md)) — `Text::CSV`
(33 files) and `CSV::Table` (10 files) are only the two modules this survey
happened to probe with; grep for `qq:to`/`Q:to`/`q:to` heredocs following a
`my` inside a `sub` elsewhere in the batteries corpus before assuming this is
the only place it bites.

### `Text::CSV` also needs real slang support — a second, harder blocker

Even after the heredoc bug is fixed, `Text::CSV.rakumod` itself (not just its
tests) has `use Slang::Tuxic;` at the top, and mutsu has no slang-switching
architecture (`CLAUDE.md`'s own "Raku's context-dependent parsing (slangs)"
section: "The parser does not natively support slang switching"). Confirmed
by reduction — `mutsu -I lib -e 'use Text::CSV; say "ok"'` fails to parse
before even reaching user code:

```
===SORRY!=== Error while compiling -e
expected statement: expected expected statement: expected right-hand
expression after '=' or Confused. no space allowed between method name and
the left parenthesis or expression statement
```

`Slang::Tuxic` itself is a one-purpose module: "allow whitespace between
subroutine and the opening parenthesis" — i.e. it lets the author write
`func (args)` instead of `func(args)`, purely a personal style preference,
used pervasively in `Text::CSV.rakumod` (~500 call sites match the spaced
form, though many of those are inside string literals for error messages
rather than real syntax). It is not load-bearing for CSV parsing logic.
Hand-patching the vendored source to remove the spaces would work around it,
but that conflicts with "vendor verbatim, do not hand-edit" and would need
re-doing on every upstream bump — not attempted here. **The heredoc fix alone
does not unblock `Text::CSV`**; it still needs either real slang-switching
support (a standing architectural gap tracked independently of this survey)
or a deliberate decision to hand-patch the ~500 call sites, which is not
recommended.

### `CSV::Parser` — works today, unusually thin field otherwise

```raku
use CSV::Parser;
my $fh = open 'data.csv', :r;
my $parser = CSV::Parser.new(:file_handle($fh), :contains_header_row);
my %row = %($parser.get_line());
```

The **only candidate in the field that is not blocked** — `use CSV::Parser`
loads cleanly (no heredoc, no slang) and its own suite is **5/5 under both
raku and mutsu**, unmodified. But it comes with real weaknesses that keep it
from being an obvious "just bundle it" pick:

- **License is declared only in the README**, not `META6.json` and not a
  `LICENSE` file: "This library is free software; you can redistribute it
  and/or modify it under the Artistic License 2.0." Per
  [selection-method.md](selection-method.md)'s hard gate ("no declared
  license anywhere → out"), a README statement counts as *declared*, so this
  does not disqualify it outright the way `Text::CSV::LibCSV`'s total silence
  does — but it is thinner evidence than every other bundled battery has, and
  worth getting the author to add a proper `LICENSE` file / `META6.json`
  field before shipping it, rather than relying on prose that could be edited
  or dropped in a future release.
- **Test coverage is thin**: 5 files, 1 assertion apiece (`t/01_multiline_csv.t`
  … `t/05_normalizer.t`). It does cover the right shapes (multiline quoted
  fields, escaped quotes, custom delimiters, binary mode, a field normalizer
  callback) but nowhere near the depth `Text::CSV`'s 22697-assertion suite
  does.
- **Zero dependents, single author (`tony-o`), last released 2023-06-06** —
  no ecosystem signal that anyone relies on it, and the README's own tone
  ("This module is pretty badass... parse your binary CSV files like a pro")
  reads as a personal utility rather than a maintained library aimed at
  general reuse.
- **API is minimal**: line-at-a-time `get_line()` returning a `Hash`/positional
  parse, no writer/composer half (no `Text::CSV`-style `combine`/`getline_all`/
  round-trip story) — reading only.

It is the pragmatic "unblock something today" option, not a confident
long-term pick.

## Ruled out before a full measurement

- **`Text::CSV::LibCSV`** (0.0.3, `tony-o`) — **no license declared anywhere**:
  no `LICENSE` file, no `META6.json` `license` field, no README mention.
  Per the selection-method.md hard gate, this is disqualified outright — the
  exact precedent that dropped `HTML::Template`/`Text::Template` from the
  template slot. Also structurally heavier than the other options regardless:
  it build-compiles a native `libcsvwrap.so`/`.dll`/`.dylib` C wrapper around
  `libcsv` (`build-depends: LibraryMake`), i.e. a NativeCall + system-library
  dependency for a task the pure-Raku candidates handle without one.
- **`Duck::CSV`** (0.0.2, MIT) — depends on `Duckie`, a NativeCall binding to
  **DuckDB**, a full embedded analytical database engine. Wildly
  disproportionate machinery for "read/write a CSV file" — the kind of
  heavyweight native dependency this slot should specifically avoid when
  pure-Raku options exist and are healthy under raku. Not installed/measured
  for that reason.
- **`CSV-AutoClass`** (0.2.0, Artistic-2.0, `tbrowder`) — a narrow
  code-generation utility (define a class whose attributes come from a CSV
  file's header row) layered on `CSV::Parser`, not a general CSV
  reader/writer. Out of scope for this slot; would only matter if `CSV::Parser`
  itself were chosen and someone wanted the codegen convenience on top.
- **`JSON-CSV`** (0.0.1, Artistic-2.0) — a CSV↔JSON conversion script package
  (`bin/csv2json` etc.), not a library API; it depends on `Text::CSV` anyway,
  so it inherits that candidate's blockers and isn't a distinct option.

## Recommendation

1. **Fix the heredoc-in-sub-body false positive first**
   (`todo/tickets/heredoc-scope-check-false-positive-on-sub-body.md`). It is a
   general compiler bug, not a CSV-specific one, and it is the single blocker
   standing between mutsu and `CSV::Table` (10/10 under raku, 0/10 today) —
   plausibly cheap, on the same "one bug unblocks a healthy module" shape the
   template slot found with `Template::Mustache`.
2. **Re-measure `CSV::Table` after that fix lands.** If it comes up clean, it
   is a stronger long-term candidate than `CSV::Parser`: real dependents-free
   ecosystem standing is a wash either way (both 0), but `CSV::Table` has 46×
   the test depth (184 vs 5 assertions) and a richer table/matrix API
   (row/column slicing, save-back, header handling) versus `CSV::Parser`'s
   bare line-at-a-time reader.
3. **`Text::CSV` stays blocked on a second, harder problem** (slang support)
   even after the heredoc fix, so it is not the near-term unblock candidate
   despite being the most CPAN-familiar API (`Text::CSV_XS`-alike) and having
   by far the deepest test suite (22697 assertions). Revisit once/if
   slang-switching architecture exists for other reasons.
4. **If a CSV story is needed before either fix lands**, `CSV::Parser` is the
   only thing that works today — zero deps, passes its own (thin) suite
   unmodified — but its license-in-README-only status and single-author,
   no-dependents standing mean it should be treated as a stopgap, not a
   final answer, and re-surveyed once the heredoc fix unblocks the stronger
   candidates.

No candidate is bundled as of this writing; this document exists to make the
next pass at the slot start from measurements instead of guesswork.
