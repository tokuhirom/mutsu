# `DBIish` battery — remaining blockers

The database battery is selected but not yet bundled; the reasoning and the
candidate comparison are in [docs/batteries/database.md](../../docs/batteries/database.md).
This file is the ledger of what stops `DBIish` from running on mutsu. Measured
2026-07-25, `DBIish` 0.6.8, debug build of `main`, and re-measured the same day
after the parse blocker was fixed.

Only the generic and SQLite files are in scope — `libpq` / `libmysqlclient` are
not installed on the survey machine, so the Pg/MySQL/Oracle/SQLCipher files are
neither passing nor failing.

## Reproducing

```sh
mkdir -p tmp/dbslot && cd tmp/dbslot
for u in \
 'https://raw.githubusercontent.com/raku/REA/main/archive/D/DBIish/DBIish%3Aver%3C0.6.8%3E%3Aauth%3Czef%3Araku-community-modules%3E%3Aapi%3C1%3E.tar.gz' \
 'https://raw.githubusercontent.com/raku/REA/main/archive/N/NativeHelpers%3A%3ABlob/NativeHelpers%3A%3ABlob%3Aver%3C0.1.9%3E%3Aauth%3Cgithub%3Asalortiz%3E.tar.gz' \
 'https://raw.githubusercontent.com/raku/REA/main/archive/N/NativeLibs/NativeLibs%3Aver%3C0.0.9%3E%3Aauth%3Czef%3Araku-community-modules%3E.tar.gz' ; do
 curl -sSL "$u" | tar xz; done
cd DBIish-0.6.8
INC=(-I lib -I ../NativeLibs-0.0.9/lib -I ../NativeHelpers-Blob-*/lib)
export DBIISH_WRITE_TEST=YES        # required, or the write tests all skip
raku $INC t/45-sqlite-common.rakutest      # baseline: 9/9 files pass
mutsu $INC t/45-sqlite-common.rakutest
```

**`$INC` must be a shell array.** zsh does not word-split a plain scalar, so
`raku $INC …` passes one giant argument and every file "fails" under raku too —
a bogus baseline that wastes a session.

## Status: mutsu 6/9 (raku parity on 6 of the 9)

Re-measured **2026-07-26** with `tmp/dbiish-survey.sh` (in this repo's `tmp/`,
recreate it from the recipe above), debug build, both interpreters on the same
`-I` line, **after ② and ⑥ were fixed**.

| File | raku | mutsu | Blocker |
| --- | --- | --- | --- |
| `02-meta` | PASS 1/1 | **PASS 1/1** | — |
| `46-sqlite-blob` | PASS 18/18 | **PASS 18/18** | — |
| `48-sqlite-errors` | PASS 17/17 | **PASS 17/17** | — |
| `44-sqlite-memory` | 1 fail of 109* | **PASS 109/109** | — |
| `45-sqlite-common` | 1 fail of 109* | **PASS 109/109** | — |
| `03-lib-util` | 1 fail of 5* | 1 fail of 5* | — (same subtest as raku) |
| `01-basic` | PASS 35/35 | 3 fail of 18 run | ⑧ a second `require ::($m)` of a driver loses `NativeLibs`' exports |
| `05-mock` | PASS 16/16 | 1 fail of 16 | ④b `IterationEnd` from a row fetch (test 12) |
| `06-types` | PASS 12/12 | 2 fail of 3 run | ⑤ `Int is builtin` / `So not defined`; mutsu suggests `Did you mean 'invert'?` |

\* Those raku failures are `# TODO`-marked and environment-dependent, not bugs:
`03-lib-util` test 5 fails on both because `libpq` is not installed on the survey
machine, and `44-`/`45-` test 52 is a `rows()` capability check raku itself marks
`# TODO`. mutsu passes test 52. The achievable target is raku parity, not
109/109 — six files are now there.

**Nothing fails inside NativeCall**: the surface `OpenSSL` needs (CStruct,
opaque pointers, callbacks) is strictly harder than SQLite's, and it is holding.

## The first round of these numbers was taken with the wrong `NativeLibs`

`-I` used not to override an installed module of the same name (raku's does), so
every run in the first survey loaded the site repo's `NativeLibs` **0.0.8**
instead of the 0.0.9 the `-I` line pins — a differently-shaped `cannon-name`.
That is fixed
([`news/2026-07/dash-i-beats-installed-modules.md`](../../news/2026-07/dash-i-beats-installed-modules.md));
the tell was a stack frame pointing into
`~/.local/share/mutsu/repo/site/sources/…`, and those frames now name
`../NativeLibs-0.0.9/lib/NativeLibs.rakumod`. The table above is the re-run.

## ① Parse failure — FIXED, was worth four files

`Failed to parse module 'DBIish::CommonTesting': X::Comp::Group: Missing block`.

A class declared inside a `package` block was not a type name to the parser, so
the `when X::DBIish::LibraryMissing { … }` in `CommonTesting`'s `CATCH` could not
parse. Fixed — see
[`news/2026-07/package-nested-class-is-a-parser-type-name.md`](../../news/2026-07/package-nested-class-is-a-parser-type-name.md).
All four affected files now parse and reach their TAP plan; they fail later, on
② below.

## ② `Unknown function: cannon-name` — worth **five** files

All five now fail with the same message. Getting them here took two steps.

**Cleared: the `NativeHelpers::Blob` load.** Four of them used to die earlier,
inside a `CHECK`, because `NativeHelpers::Blob` could not be loaded at all: its
`MoarVM::Guts::REPRs` needs `nativesizeof`, a dereferenceable `Pointer.WHERE`,
positional `Pointer.new` and reads through a `nativecast`ed `CArray` handle —
none of which mutsu had. Those are in now; see
[`news/2026-07/nativecall-sizeof-and-pointer-where.md`](../../news/2026-07/nativecall-sizeof-and-pointer-where.md).
The *rest* of that module — `BODY_OF` / `pointer-to()`, which hand C the address
of a container's element buffer — needs a stable native allocation behind
`Blob`/`array`/`CArray`, i.e. a value-representation change with its own design
work. That half stays in
[`todo/deep/nativehelpers-blob-moarvm-guts.md`](../deep/nativehelpers-blob-moarvm-guts.md);
`DBDish::SQLite` only uses `blob-from-pointer`, which does not go through it.

**Cleared: `cannon-name` itself — FIXED 2026-07-26.** It had nothing to do with
`proto`/`multi`, `sub EXPORT`, or NativeCall. `cannon-name` is only ever called
from *inside* `NativeLibs.rakumod`, at lines 131 and 134, which are in a method
of `class Searcher`, and **a class declared inside a `module` could not see that
module's subs**: bare-name lookup jumped straight from the current package to
`GLOBAL`, and a method body did not even run under its own class's package. Both
are fixed — see
[`news/2026-07/class-in-module-sees-module-subs.md`](../../news/2026-07/class-in-module-sees-module-subs.md).
All five files moved off this blocker; two of them (`46-sqlite-blob`,
`48-sqlite-errors`) now pass outright.

The earlier reductions recorded here were not wrong so much as aimed one row off
the failing case — they all call from a sibling *sub*, which works. (They were
also checked while the installed 0.0.8 was being loaded.)

## All four remaining blockers, reduced (2026-07-26)

Each of ③④⑤⑦ now has a standalone repro that does not involve `DBIish`,
`NativeLibs` or a database. Keep them here rather than in `tmp/` — that directory
is gitignored and the LXC container is disposable. Paste the block into a file
and run it under both interpreters; raku prints every line without dying.

```raku
# --- (4a) a bare adverb on a listop argument swallows the rest of the list ---
class H { method row(:$hash) { $hash ?? 'HASH' !! 'LIST' } }
my $h = H.new;
sub show(*@a) { @a.join('|') }
say "4a-control: ", (try show($h.row(:hash), 'x', 'y')) // "DIED: $!";
say "4a-bare   : ", (try show($h.row :hash, 'x', 'y')) // "DIED: $!";

# --- (4b) pull-one on an iterator taken from a user-produced Seq ---
class S { method allrows() { gather { take ['a','b',1]; take ['d','e',2] } } }
my \a = S.new.allrows;
say "4b-isa    : ", a.^name;
say "4b-pullone: ", (try a.iterator.pull-one.raku) // "DIED: $!";

# --- (3) ClassHOW.method_table ---
class M { method connect() { }; method install-driver() { } }
say "3-mtable  : ", (try M.^method_table<connect>:exists) // "DIED: $!";

# --- (7) $*VM.config<nativecall_backend> ---
say "7-nc-back : ", (try $*VM.config<nativecall_backend>.raku) // "DIED: $!";
say "7-keys    : ", $*VM.config.keys.sort.join(',');
```

| line | raku | mutsu |
| --- | --- | --- |
| `4a-control` | `HASH\|x\|y` | `HASH\|x\|y` |
| `4a-bare` | `HASH\|x\|y` | `HASH\|x\|y` (fixed) |
| `4b-isa` | `Seq` | `Seq` |
| `4b-pullone` | `["a", "b", 1]` | **`"IterationEnd"`** |
| `3-mtable` | `True` | `True` (fixed) |
| `7-nc-back` | `"dyncall"` | `"libffi"` (fixed) |
| `7-keys` | ~200 keys | `be,name,nativecall_backend` |

### ④a A bare adverb on a listop argument — FIXED

`is-deeply $sth.row :hash, hash(...), 'desc'` handed `.row` the two following
arguments instead of leaving them to the listop. The method-call parser already
told the colon call (`.m: a, b`, which does take the comma list) apart from the
space-separated adverb, but both shared one continuation loop that kept
consuming `, next`. Fixed — see
[`news/2026-07/method-table-and-hash-composer-parse.md`](../../news/2026-07/method-table-and-hash-composer-parse.md).

### ④b `pull-one` on a hand-obtained iterator yields the `IterationEnd` sentinel

Three lines of `gather`/`take` reproduce it: `a.iterator.pull-one` answers the
string `IterationEnd` instead of the first element. This is `05-mock` test 12.

Reduced further 2026-07-26: it is specific to a **lazy, not-yet-materialised**
source. `(1,2,3).Seq.iterator.pull-one`, `@a.iterator.pull-one` and
`(1..3).map(*+1).iterator.pull-one` are all correct; only `gather`/`take` fails,
and forcing it first (`$s.elems; $s.iterator.pull-one`) makes it correct too. The
cause is that `builtins::iterator_construct::build_iterator_instance` snapshots
`value_to_list(target)` into an `items` array — for a lazy gather that prefix is
empty, so `runtime/iterator_protocol.rs` steps straight past the end. The real
fix is an `Iterator` that pulls from its source on demand rather than from a
materialised prefix; eagerly forcing the source instead would hang on an
infinite lazy list.

### ③ `.^method_table` — FIXED

Not a `PackageHOW`-only gap as first recorded — a plain `ClassHOW` had the same
hole. Fixed, with `Method` objects as the values, matching rakudo. See
[`news/2026-07/method-table-and-hash-composer-parse.md`](../../news/2026-07/method-table-and-hash-composer-parse.md).

### ⑧ A repeat `require ::($module)` loses `NativeLibs`' re-exports

New, and what `01-basic` stops on now. Installing the drivers one at a time works
(`DBIish.install-driver('SQLite')` on its own returns `DBDish::SQLite`), but the
file's `for <Oracle Pg SQLite TestMock mysql>` loop fails on the third and fifth:
`Could not find symbol '&is-win' in 'NativeLibs'` for SQLite and `Type 'ulong' is
not declared` for mysql, both raised from inside `NativeLibs`' `CHECK for
NativeCall::EXPORT::.keys { UNIT::EXPORT::{$_} := … }`. So a second `require
::($module)` in the same process re-runs that `CHECK` against a registry that no
longer holds the first load's exports — the same export/registry-rewind family as
`news/2026-07/`'s subtest and `EVAL` entries.

### ⑤ `06-types` — object hash keyed by type objects

Not a spell-correction problem. The role under test is

```raku
role TypeConverter does Associative {
    has Callable %!Conversions{Mu:U} handles <AT-KEY EXISTS-KEY>;
    ...
}
```

and the test declares `has %.Converter is DBDish::TypeConverter;`, then
`%!Converter{Str} = self.^find_method('test-str')`. So the file needs: an
**object hash keyed by `Mu:U`** (type objects as keys), `handles <AT-KEY
EXISTS-KEY>` delegation on a private attribute, an attribute typed with a role,
`.^find_method`, and the indirect method call `$test.$sub('test')`. Start by
checking which of those five mutsu lacks — the `Did you mean 'invert'?`
suggestion is the *symptom* of an unresolved delegated `AT-KEY`, not a typo.

Note the object-hash requirement overlaps the deferred "object-hash `WHICH`"
item in the doc-diff DEEP bucket (`docs/doc-diff-backlog.md`).

### ⑦ `$*VM.config` — `nativecall_backend` added, the rest of the surface is not

`NativeLibs` reads
`my \dyncall = $*VM.config<nativecall_backend> eq 'dyncall'` and used to warn
`Use of uninitialized value of type Any in string context` on every run that
loads it — noise that had twice been mistaken for a diagnosis in this file. That
key now answers `"libffi"`, which is what mutsu's FFI actually is and what a
modern MoarVM reports (`dyncall` still ends up `False`, which is what mutsu
wants). The config still has three keys against raku's ~200; deciding how much
of that surface to synthesise is a separate question, and nothing in `DBIish`
needs it.

## Role attribute not seeded — FIXED

```
P6opaque: no such attribute '$!parent' on type DBDish::ErrorHandling in a DBDish::ErrorHandling
```

`DBIish` instantiates the `DBDish::ErrorHandling` role directly
(`DBDish::ErrorHandling.new(:parent(Nil))`), which puns it to a class, and its
methods read those attributes privately. A punned role kept its attributes only
as mixin markers, so the private read found nothing. Fixed — see
[`news/2026-07/role-pun-private-attribute.md`](../../news/2026-07/role-pun-private-attribute.md).

**Read this before forming a theory about ⑤:** its first non-TAP line is only a
*warning* (`Use of uninitialized value of type Str in string context`, from the
test file's own `BUILD`), emitted by both implementations. The same trap already
cost a session on `Template::Mustache` — get the real failing assertion first.

## ⑥ `.^ver` of a class declared with a computed `:ver(<expr>)` — FIXED

Was the one subtest each that mutsu failed on `44-sqlite-memory` and
`45-sqlite-common`; both now pass 109/109. Fixed — see
[`news/2026-07/computed-declarator-adverb.md`](../../news/2026-07/computed-declarator-adverb.md).
The failing assertion was test 2 of `DBIish::CommonTesting`:

```raku
my $aversion = $drh.Version;
ok $aversion ~~ Version:D, "DBDish::{$.dbd} version $aversion";
```

`$.Version` comes from `role DBDish::Driver`'s `has $.Version = ::?CLASS.^ver`,
and `DBDish::SQLite` is declared
`unit class DBDish::SQLite:ver($?DISTRIBUTION.meta<ver>):api(...):auth(...)`.
With a plain `-I lib` there is no distribution, so the `:ver` expression
evaluates to an undefined value — raku still hands back a **defined** `Version`
(`Version.new('*')` here, `Version.new` for `:ver(Nil)`), mutsu hands back `Mu`,
so the `Version:D` check fails and the description renders empty. `.^ver` for a
*literal* `:ver<1.2.3>` is correct in mutsu; only the computed form is wrong.
Note the plain `class A {}` case: raku's `A.^ver` really is `Mu`, so the fix is
specifically "an explicit `:ver(<expr>)` always yields a `Version`", not "default
`.^ver` to something defined".

The defect was one level down: **mutsu did not evaluate the `:ver(...)`
expression at all, it stored its source text** — and the `unit class` form threw
the adverbs away outright.

## Suggested order for the next session

⑦, ③ and ④a are done. What is left, cheapest first; none depends on another:

1. **④b** — the `IterationEnd` leak, now reduced to "a lazy source hands the
   `Iterator` an empty materialised prefix". Needs a pull-on-demand `Iterator`,
   so it is a real slice, not a one-liner.
2. **⑧** — a repeat `require ::($m)` losing `NativeLibs`' re-exports. Related
   registry-rewind bugs have been fixed twice before, so there is a model.
3. **⑤** — the largest: an object hash keyed by type objects, plus `handles`
   delegation from a private attribute. Confirm which of the five features listed
   above is actually missing before scoping it.

## When these are cleared

Follow the "Next steps before this can be bundled" list at the end of
`docs/batteries/database.md`: re-measure, vendor the three trees, add them to
`batteries.lock`, and baseline the release gate with
`scripts/battery-testsuite.sh --update`.
