# `Text::CSV` is bundled — CSV read/write with no install

mutsu now ships Tux's `Text::CSV` distribution (`zef:Tux`, v0.022,
Artistic-2.0) in `modules/Text-CSV/`, closing the CSV battery slot flagged as
gap #2 in the Python-stdlib comparison — a plain `use` works with no
`zef install` and no network:

```raku
use Text::CSV;
my @rows = csv(in => "data.csv");                              # read
csv(in => [["name","score"], ["ana","42"]], out => "out.csv"); # generate
my $csv = Text::CSV.new;
$csv.parse('one,"two, with comma",three');                     # line-level
$csv.combine("a", "b,c", "d");  $csv.string;                   # a,"b,c",d
```

## Why this dist

The survey (`docs/batteries/csv.md`) narrowed the field to two pure-Raku
read-and-generate candidates. `Text::CSV` won on both remaining axes: it is
the only candidate that builds a CSV from in-memory data with no pre-existing
template file (`CSV::Table`'s constructor requires an existing `:csv($file)`),
and it is the deepest-tested by two orders of magnitude (22,696 assertions
against 184). Its once-hard blocker — `use Slang::Tuxic` at the top of the
module — was already paid for by the ADR-0026 slang-activation campaign,
which bundled `Slang::Tuxic` and `Slangify`; `File::Temp` was already aboard
for the HTTP slot, so this bundling added zero new dependencies.

## What it cost

The measurement campaign that made the suite green (32/32 files; `99_meta.t`
waived by user decision, `Test::META` is dist-metadata QA) ran through
several dozen general interpreter fixes over three weeks — slang activation
machinery, the heredoc-scope false positive, typed-lexical constraint frame
scoping (#6354), and friends. The bundling itself surfaced one more: the
four-line smoke test exposed the expression-position `my` caller-lexical
leak, fixed in the same PR
(`news/2026-08/expr-decl-lexical-leak.md`).

## Gate status

All 32 functional upstream test files are whitelisted and run on every
release against the bundled tree (fetched fresh at the pinned `v0.022`
commit, per `batteries.lock`). `99_meta.t` needs the unbundled `Test::META`
and simply stays off the whitelist. Smoke pin: `t/text-csv-battery.t`,
resolved with no `-I`.
