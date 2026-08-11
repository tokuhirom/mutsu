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

**Selection criteria for this slot (user decision, 2026-08-11):** no
external native/C-library dependency (rules out `Text::CSV::LibCSV`'s
`libcsv` wrapper and `Duck::CSV`'s DuckDB binding outright — see "Ruled out"
below, which independently arrived at the same two rejections on license/
scope grounds before this criterion was stated), and the candidate must
support **both reading and generating** CSV data, not reading alone. This
last point demotes `CSV::Parser` from "pragmatic stopgap" to **disqualified**
— see its section below, updated accordingly.

## The field

Enumerated from `~/.zef/store/{rea,fez}/*.json` (the same indices `mzef`
uses) by filtering name/description/tags on `csv`. `App::Rak` and
`Data::Reshapers`/`Data::Summarizers` matched the keyword search but are
general-purpose tools (a grep-alike, a data-reshaping toolkit) that merely
support CSV as one input format among several — excluded as not CSV-slot
candidates.

| Candidate | Version | Released | License | Runtime deps | Read+generate? | Native/C dep? | Dependents¹ | raku | **mutsu** |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **`Text::CSV`** | 0.022 | 2023-10-30 | Artistic-2.0 | `Slang::Tuxic`, `File::Temp` | ✅ both, incl. from-scratch `csv(out=>…)` | none | 0 | **33/33** (22697 tests) | **0/33** — blocked at `use` |
| **`CSV::Table`** | 0.0.2 | 2025-05-31 | Artistic-2.0 | `YAMLish`, `JSON::Fast`, `File::Temp`, `Text::Utils` | ✅ both, but write needs an existing file to load first | none | 0 | **10/10** (184 tests) | **0/10** — blocked at `use` |
| `CSV::Parser` | 0.1.4 | 2023-06-06 | *(README only — see below)* | **0** | ❌ read-only — **disqualified** | none | 0 | **5/5** | **5/5** ✅ |
| `CSV-AutoClass` | 0.2.0 | 2023-11-19 | Artistic-2.0 | `CSV::Parser`, `File::Find`, `Text::Utils` | ❌ codegen utility, not a reader/writer | none | 0 | 0/2 (missing test fixture on this checkout) | 0/2 — blocked at `use` |
| `Duck::CSV` | 0.0.2 | 2026-05-30 | MIT | `Duckie` (→ system `libduckdb`) | not evaluated — disqualified below | **DuckDB (native)** | 0 | not measured² | not measured² |
| `Text::CSV::LibCSV` | 0.0.3 | 2022-09-09 | **none declared anywhere** | native `libcsv` (build-time C compile) | not evaluated — disqualified below | **libcsv (native)** | 0 | 0/5 — missing test fixtures | not measured³ |
| `JSON-CSV` | 0.0.1 | 2022-07-11 | Artistic-2.0 | `Text::CSV`, `JSON::Fast`, `JSON::Stream` | conversion scripts, not a library API | none | 0 | not measured | not measured |

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

### `CSV::Parser` — works today, but disqualified: read-only

```raku
use CSV::Parser;
my $fh = open 'data.csv', :r;
my $parser = CSV::Parser.new(:file_handle($fh), :contains_header_row);
my %row = %($parser.get_line());
```

The **only candidate in the field that is not blocked** — `use CSV::Parser`
loads cleanly (no heredoc, no slang) and its own suite is **5/5 under both
raku and mutsu**, unmodified. Under this slot's read-*and*-generate
criterion it is disqualified anyway: its **API is read-only** —
line-at-a-time `get_line()` returning a `Hash`/positional parse, with no
writer/composer half at all (no `Text::CSV`-style `combine`/`print`/`csv(out
=> …)` round-trip story). It also carries weaker secondary signals than
either read+write candidate: license declared only in its README (not
`META6.json`/`LICENSE`), 5 thin test files (1 assertion apiece) against
`Text::CSV`'s 22697 assertions, and zero dependents from a single author
whose README tone ("This module is pretty badass... parse your binary CSV
files like a pro") reads as a personal utility. Kept in the table for
completeness, not as a candidate to bundle.

## Ruled out before a full measurement

- **`Text::CSV::LibCSV`** (0.0.3, `tony-o`) — **no license declared anywhere**:
  no `LICENSE` file, no `META6.json` `license` field, no README mention.
  Per the selection-method.md hard gate, this is disqualified outright — the
  exact precedent that dropped `HTML::Template`/`Text::Template` from the
  template slot. Independently disqualified by the no-native-dependency
  criterion too: it build-compiles a native `libcsvwrap.so`/`.dll`/`.dylib` C
  wrapper around `libcsv` (`build-depends: LibraryMake`), i.e. a NativeCall +
  system-library dependency for a task the pure-Raku candidates handle
  without one.
- **`Duck::CSV`** (0.0.2, MIT) — depends on `Duckie`, a NativeCall binding to
  **DuckDB**, a full embedded analytical database engine. Disqualified by the
  no-native-dependency criterion — wildly disproportionate machinery for
  "read/write a CSV file" when pure-Raku options exist and are healthy under
  raku. Not installed/measured for that reason.
- **`CSV-AutoClass`** (0.2.0, Artistic-2.0, `tbrowder`) — a narrow
  code-generation utility (define a class whose attributes come from a CSV
  file's header row) layered on `CSV::Parser`, not a general CSV
  reader/writer. Out of scope for this slot; would only matter if `CSV::Parser`
  itself were chosen and someone wanted the codegen convenience on top.
- **`JSON-CSV`** (0.0.1, Artistic-2.0) — a CSV↔JSON conversion script package
  (`bin/csv2json` etc.), not a library API; it depends on `Text::CSV` anyway,
  so it inherits that candidate's blockers and isn't a distinct option.

## Recommendation

Under the stated criteria (no native/C-library dependency, real
read-and-generate API), the field narrows to exactly two live candidates —
`Text::CSV` and `CSV::Table` — both pure Raku, both blocked purely by mutsu
bugs rather than by anything wrong with the module itself:

1. **Fix the heredoc-in-sub-body false positive first**
   (`todo/tickets/heredoc-scope-check-false-positive-on-sub-body.md`). It is a
   general compiler bug, not a CSV-specific one, and it is the **only**
   blocker standing between mutsu and `CSV::Table` (10/10 under raku, 0/10
   today), and one of two blockers for `Text::CSV`. Plausibly cheap, on the
   same "one bug unblocks a healthy module" shape the template slot found
   with `Template::Mustache`.
2. **Re-measure both `Text::CSV` and `CSV::Table` after that fix lands.**
   - `Text::CSV` has the fuller generation story: `combine`/`print`/`say` for
     line-at-a-time composition plus a top-level `csv(in => @data, out =>
     $file)` functional form that builds a CSV file directly from an
     in-memory data structure — no pre-existing file needed. It is also the
     deeper-tested candidate by far (22697 vs 184 assertions) and the most
     CPAN-familiar API (`Text::CSV_XS`-alike). But it **stays blocked on a
     second, harder problem** even after the heredoc fix: `use
     Slang::Tuxic;` at the top of `Text::CSV.rakumod` itself needs real
     slang-switching support, a standing architectural gap (see "What blocks
     mutsu today" above) — not something to build just for this slot.
   - `CSV::Table` clears with the heredoc fix **alone**, making it the
     nearer-term option. Its write side is real (`.save`, cell mutation,
     `save($stem)` to a new path) but structurally anchored: the class
     constructor requires an *existing* `:csv($file)` to load
     (`has $.csv; #is required;`, `lib/CSV/Table.rakumod:7`) — there is no
     from-scratch "build a CSV out of `@data-structure` alone" entry point
     the way `Text::CSV`'s functional `csv()` sub has. In practice this means
     "generate a new CSV" with `CSV::Table` means starting from a (possibly
     header-only or otherwise minimal) template file, not calling a
     constructor with just Raku data. Worth confirming this is acceptable
     for the actual use cases this slot is meant to serve before treating it
     as equivalent to `Text::CSV`'s generation story.
3. **No stopgap is recommended before the heredoc fix lands.** `CSV::Parser`
   would have been the "unblock something today" pick, but it is read-only
   and therefore disqualified by this slot's own criteria — bundling it now
   would mean re-surveying and likely replacing it shortly after, which is
   worse than simply waiting for the shared compiler fix.

No candidate is bundled as of this writing; this document exists to make the
next pass at the slot start from measurements instead of guesswork.
