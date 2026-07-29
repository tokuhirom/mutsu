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

## Status: **9/9 — raku parity on every file** (2026-07-28)

**Update 2026-07-29: mutsu opens a real MariaDB connection.** These nine files
only ever *load* the mysql driver; running it against a server took eight more
NativeCall/dispatch fixes
([one](../../news/2026-07/nativecall-cglobal-and-native-methods.md),
[two](../../news/2026-07/cstruct-handles-carry-their-registered-name.md)).
`DBDish::mysql.new.connect(…)` returns a `DBDish::mysql::Connection` and
`.prepare` returns a real `StatementHandle`.

`execute` reading `$!stmt` with the wrong invocant is fixed:
[`self` is lexical inside a block](../../news/2026-07/self-is-lexical-in-blocks.md)
even when the block runs inside another object's method, which is what
`$!parent.protect-connection: { $!stmt… }` does. `DBDish::Pg` shares
`DBDish::StatementHandle`, so it was going to hit the same line.

**Update 2026-07-29 (later): prepared `INSERT`s run against the server.**
`$insert.execute('BUBH', 'Hot beef burrito', 1, 4.95)` reaches MariaDB and
`$insert.rows` comes back. Three more fixes were needed, all of them general:

- [a CStruct field type follows a `constant` alias](../../news/2026-07/cstruct-fields-follow-constant-type-aliases.md).
  `MYSQL_BIND` declares `has intptr $.length`, `intptr` is not a NativeCall type
  name, and one unmappable field aborts the whole layout — so `MYSQL_BIND` had
  no layout, `nativesizeof` on it failed, and `LinearArray[MYSQL_BIND]` (whose
  role body computes its stride with `nativesizeof(T)`) came back
  *unparameterised*. The tell: `.REPR` said `CStruct` while `nativesizeof` said
  `P6opaque`.
- [parameterising a role no longer retopicalizes the caller](../../news/2026-07/role-parameterisation-keeps-the-caller-topic.md).
  The type-argument expression and the role's deferred body each published their
  value through `$_`, so the *first* `LinearArray[MYSQL_BIND].new($pc)` inside
  `with $!stmt { … }` left the topic as `MYSQL_BIND` and the next
  `.mysql_stmt_field_count` ran on the wrong invocant.
- [`$*VM.platform-library-name` honours `:version`](../../news/2026-07/platform-library-name-keeps-its-version.md)
  — needed by `DBDish::Pg`, see below.

**Reading rows back** used to fail on the mysql end-to-end path
(`tmp/mysql-e2e-use.raku`) with

```
No such method 'convert-function' for invocant of type 'Hash'
  in sub _row ... in sub row ...
```

`DBDish::StatementHandle`'s `_row` binds `my %Converter := $!parent.Converter`
and found a plain `Hash` where a `TypeConverter` was expected. This was *not* the
punned-role-in-an-attribute store of ⑤ (that landed 2026-07-26); it was the
container **tie** in front of it — `has %.Converter is DBDish::TypeConverter`
populated by a bare `%!Converter = …` in BUILD. Fixed in
[`news/2026-07/tied-container-attribute-and-role.md`](../../news/2026-07/tied-container-attribute-and-role.md):
a whole-value assignment to an attribute now routes through `STORE` instead of
replacing the tie, and a tie named by a *role* is recognised at all.

Still open before `DBIish.connect(…)` works through its own front door:
[`require-loaded-module-loses-use-imports.md`](require-loaded-module-loses-use-imports.md).

**`01-basic` regressed to 12 of 35 in this environment, and it is not a mutsu
regression** — `libpq5` was installed on the survey machine after the 35/35
measurement, so the file now actually exercises the Pg driver. The first Pg
blocker (`platform-library-name` dropping `:version`) is fixed above; re-measure
before reading anything else into that number.

A gap found while pinning the CStruct-alias fix, deliberately left out of scope:
`nativecast(SomeCStruct, $carray)` on a **Raku-side** `CArray` (as opposed to a
handle C gave us) produces an instance with no address, so reading a field off
it fails with "No such method". Every DBIish path casts a pointer that came from
C, so nothing here needs it.

The three fixes that closed the last file are recorded in
[`news/2026-07/buf-repr-body-and-native-storage.md`](../../news/2026-07/buf-repr-body-and-native-storage.md),
[`ternary-then-branch-enum-value.md`](../../news/2026-07/ternary-then-branch-enum-value.md)
and
[`hyper-descends-into-an-itemized-list.md`](../../news/2026-07/hyper-descends-into-an-itemized-list.md).
What follows is the history of how it got here; nothing below is open work.

### Earlier status: mutsu 8/9 (raku parity on 8 of the 9)

**Updated 2026-07-26 (late):** `06-types` now passes 12/12 — see
[`news/2026-07/punned-role-container-attribute-store.md`](../../news/2026-07/punned-role-container-attribute-store.md).
Only `01-basic` is left, and it is ⑨ (`BODY_OF`, a deferred deep item). The
table below is the earlier survey; its `06-types` row is superseded.


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
| `01-basic` | PASS 35/35 | 3 fail of 30 run | ⑨ the `mysql` driver needs `BODY_OF` (deferred deep item) |
| `05-mock` | PASS 16/16 | **PASS 16/16** | — |
| `06-types` | PASS 12/12 | 2 fail of 3 run | ⑤ `Int is builtin` / `So not defined`; mutsu suggests `Did you mean 'invert'?` |

\* Those raku failures are `# TODO`-marked and environment-dependent, not bugs:
`03-lib-util` test 5 fails on both because `libpq` is not installed on the survey
machine, and `44-`/`45-` test 52 is a `rows()` capability check raku itself marks
`# TODO`. mutsu passes test 52. The achievable target is raku parity, not
109/109 — seven files are now there.

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
| `4b-pullone` | `["a", "b", 1]` | `["a", "b", 1]` (fixed) |
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

### ④b `pull-one` on a hand-obtained iterator yielded the `IterationEnd` sentinel — FIXED

It was specific to a **lazy, not-yet-materialised** source.
`(1,2,3).Seq.iterator.pull-one`, `@a.iterator.pull-one` and
`(1..3).map(*+1).iterator.pull-one` were all correct; only `gather`/`take`
failed, and forcing it first (`$s.elems; $s.iterator.pull-one`) made it correct
too. `build_iterator_instance` snapshotted `value_to_list(target)` into an
`items` array — for an unforced gather that prefix is empty, so
`runtime/iterator_protocol.rs` stepped straight past the end.

Fixed — the `Iterator` keeps its lazy source and the protocol methods pull from
it, bounded by what the call needs, so an infinite source stays lazy. See
[`news/2026-07/iterator-pulls-from-its-lazy-source.md`](../../news/2026-07/iterator-pulls-from-its-lazy-source.md).

### ③ `.^method_table` — FIXED

Not a `PackageHOW`-only gap as first recorded — a plain `ClassHOW` had the same
hole. Fixed, with `Method` objects as the values, matching rakudo. See
[`news/2026-07/method-table-and-hash-composer-parse.md`](../../news/2026-07/method-table-and-hash-composer-parse.md).

### ⑧ `Could not find symbol '&is-win' in 'NativeLibs'` — FIXED

The first reading here ("a second `require` re-runs `NativeLibs`' `CHECK` against
a rewound registry") was aimed one level off. It was not about `require` being
repeated, nor about the `CHECK`: **a module's `our` package variables did not
survive the scope that loaded it.** `our` compiles to a `SetGlobal`, so those
live in `env`, and a sub call restores `env` wholesale on return —
`install-driver` does its `require` inside a method, so `NativeLibs`' `our
constant is-win` went with it, while `loaded_modules` kept the module marked
loaded so the next driver's `use NativeLibs` was a no-op. Three lines reproduce
it with no database:

```raku
sub f() { my \M = (require ::('Base')); }   # Base has `our constant flag = 7`
f();
use Base;
say Base::flag;          # raku: 7    mutsu (before): could not find symbol
```

Fixed, along with two gaps it was masking (`ulong` and friends were not
declarable; a role type parameter could not carry a definiteness smiley, which
kept `NativeHelpers::CStruct` from loading at all) — see
[`news/2026-07/module-our-globals-outlive-the-loading-scope.md`](../../news/2026-07/module-our-globals-outlive-the-loading-scope.md).
`01-basic` went from 18 of 35 to 30.

### ⑨ `01-basic`'s last three failures are the `mysql` driver, gated on `BODY_OF`

Not a new bug so much as a pointer to the deferred deep item.
`DBDish::mysql::StatementHandle` uses `BPointer(...)`, which is
`NativeHelpers::Blob`'s `pointer-to` and needs `BODY_OF` — the address of a
container's element buffer, stable across calls. That is
[`todo/deep/nativehelpers-blob-moarvm-guts.md`](../deep/nativehelpers-blob-moarvm-guts.md),
whose ADR is now written and accepted:
[ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
(P0 = two small NativeCall fixes, P1 = bodies over handles, P2/P3 = native-backed
container storage). `DBDish::SQLite` does not go through `BODY_OF`, which is why
the other eight files are unaffected.

**Update: ADR-0015 P0/P1 are landed and `BODY_OF` works.** The recorded symptom
above — a parse failure in `StatementHandle` (`Unexpected block in infix
position`) caused by an undeclared `BPointer` — is stale. `BPointer` now
resolves and runs all the way into `BODY_OF`, and
`NativeHelpers::CStruct`'s `LinearArray` matches raku's output exactly
(allocates, computes its stride, indexes, nativecasts, assigns element fields,
disposes).

**Update 2026-07-28: the type-qualification bug that followed is fixed** — the
builtin preludes were being captured by the host `unit module`'s package, so
`Pointer[t]` inside `NativeHelpers::Blob` named `NativeHelpers::Blob::Pointer`
and could not be parameterized (see
[`news/2026-07/unit-module-no-longer-captures-the-builtin-preludes.md`](../../news/2026-07/unit-module-no-longer-captures-the-builtin-preludes.md)).
`BPointer` now runs all the way into `BODY_OF`'s last line and stops there:

```
$ mutsu -I <NativeHelpers-Blob>/lib -e 'use NativeHelpers::Blob; BPointer(Buf.new(1,2,3))'
Cannot dereference a Pointer[Any]: not a type NativeCall can read
  in sub BODY_OF ... in sub pointer-to ... in sub BPointer ...
```

That was **ADR-0015 P2 and nothing else**: `BODY_OF` looks the body type up as
`%known-bodies{any.REPR}`, and `Buf`/`Blob` still answered `P6opaque` where raku
answers `VMArray`, so the lookup missed and `Pointer[Any]` was what got
dereferenced. (The module's own `die "Can only handle …"` guard does not fire —
it tests `type ~~ Nil`, and a missing hash key is `Any`.)

**Update 2026-07-28: P2 is landed and `BODY_OF(Buf)` works end to end** — see
[`news/2026-07/buf-repr-body-and-native-storage.md`](../../news/2026-07/buf-repr-body-and-native-storage.md).
`pointer-to($buf)` returns the buffer's own element address, and a C function
that writes through a pointer it retained is visible in Raku with no intervening
call. Two bugs had to fall for it: the missing `VMArray` body itself, and a
CStruct field accessor being shadowed by a builtin whenever the class was
declared inside a module (which made `MVMArrayB.elems` answer `1` and `.any`
build a Junction, so the module read a junction instead of a body).

**The parser blocker that followed is fixed too.**
`DBDish::mysql::StatementHandle` did not parse, on
`.buffer_type = @!column-type[$col] ~~ Blob ?? MYSQL_TYPE_BLOB !! MYSQL_TYPE_STRING`
— a ternary whose then-branch is a value of an `enum` exported by
`DBDish::mysql::Native`. See
[`news/2026-07/ternary-then-branch-enum-value.md`](../../news/2026-07/ternary-then-branch-enum-value.md).
The mysql driver installs now and `01-basic` runs its full plan: **34 of 35**,
up from 3 failed of 30 run.

**⑨ is closed.** The last assertion, `$installed>>.key.sort`, returned a nested
one-element list because a hyper method call on an itemized list treated it as a
single element instead of descending — fixed in
[`news/2026-07/hyper-descends-into-an-itemized-list.md`](../../news/2026-07/hyper-descends-into-an-itemized-list.md).
`01-basic` is **35/35, raku parity**, and with it `DBIish` is at raku parity on
all nine files.

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
`.^find_method`, and the indirect method call `$test.$sub('test')`.

**All five were measured separately 2026-07-26** (the `Did you mean 'invert'?`
suggestion is the *symptom* of `$test.Converter` still being a plain `Hash`, not
a typo, and the file's first non-TAP line is only a warning — see the note at
the end of this file). Three are done and two remain:

- **Object hash keyed by `Mu:U`** — worked for a lexical `my %h{Mu:U}` but not
  for an attribute: every element assignment stringified the key. Fixed, along
  with role attributes having no declared type at all, in
  [`news/2026-07/role-attribute-type-constraints.md`](../../news/2026-07/role-attribute-type-constraints.md).
- **`.^find_method`** and **the indirect method call `$obj.$sub('x')`** — both
  already correct.
- **`handles <AT-KEY EXISTS-KEY>` on a private attribute** — correct on a class.
  On a *punned role* it goes through a second, incompatible store, which is the
  remaining blocker below.
- **An attribute typed with a role** (`has %.Converter is TypeConverter`) — the
  `is Type` trait itself works (`Type.new` seeds the attribute), but for a role
  that means a punned-role object living inside an attribute, which is precisely
  where the second store cannot be reached.

The last of ⑤ was the two-store problem: a punned role kept its `@`/`%`
attributes in `__mutsu_attr__` mixin markers instead of the instance's attribute
cell, so an ordinary `%!h<k> = 1` inside a role method was dropped, while the
`handles` delegation path mutated the marker and wrote the rebuilt `Mixin` back
into the *caller's env variable* — a writeback that cannot reach an object held
in an attribute. The cell is the single store for every sigil now:
[`news/2026-07/punned-role-container-attribute-store.md`](../../news/2026-07/punned-role-container-attribute-store.md).

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

⑦, ③, ④a, ④b and ⑧ are done. `05-mock` is at raku parity (16/16) and `01-basic`
is at 30 of 35. Two left:

1. ~~**⑤** — `06-types`~~ **DONE 2026-07-26.** The punned role's container
   attributes and the `handles` delegation path converged on the instance cell;
   see [`news/2026-07/punned-role-container-attribute-store.md`](../../news/2026-07/punned-role-container-attribute-store.md).
2. **⑨** — the `mysql` driver, and with it `01-basic`'s last three subtests.
   The ADR it wanted is written and accepted (ADR-0015); P0/P1 have landed, and
   as of 2026-07-28 so has the last of the small named bugs in front of it (the
   `unit module` prelude capture). **The only thing left is ADR-0015 P2** —
   native-backed `Buf`/`Blob` with an honest `VMArray` `.REPR` and an `MVMArrayB`
   body. That is the campaign, not a slice. It has been surveyed against `main`:
   [`news/2026-07/adr0015-p2-buf-storage-survey.md`](../../news/2026-07/adr0015-p2-buf-storage-survey.md)
   has the measured touch count (104, not the ADR's ~91), the `.REPR` machinery
   P1 already left in place, and — the load-bearing correction —
   **`native_object_where` cannot be extended into `MVMArrayB`**: it is memoised
   by payload address, immutable and leaked, which only works because the
   CStruct/CArray bodies are all-zero past word 0. Read it before starting. See
   ⑨ above for the exact current failure.

## When these are cleared

Follow the "Next steps before this can be bundled" list at the end of
`docs/batteries/database.md`: re-measure, vendor the three trees, add them to
`batteries.lock`, and baseline the release gate with
`scripts/battery-testsuite.sh --update`.
