# Where a `t/` test file goes

`t/` is mutsu's local TAP regression suite. It had grown to **3,948 flat `.t` files**, which is past
the point where a directory listing is a usable index: `ls t/` is unreadable, and neither a human
nor an agent can answer "is there already a test for this?" without a full-text grep. This
document defines the nested layout that replaces the flat one, and the rules for placing a new
file in it.

It is the authority for placement. `CLAUDE.md` and `AGENTS.md` point here rather than repeating
the category list.

## 1. Shape

```
t/
  <category>/[<subcategory>/]<name>.t     # test files
  lib/                                   # module fixtures loaded via -I t/lib
  fixtures/                              # data fixtures, CompUnit repo trees, ...
  packages/                              # vendored helper packages (Test-Helpers)
  lib-*/  *-lib/                         # grandfathered per-test module fixtures
```

Three rules constrain it:

- **A test file lives in a category directory, never at `t/` top level.** The top level holds
  only directories, and `make check-t-layout` fails if a `.t` appears there.
- **At most two levels below `t/`.** `t/oo/role/punning.t` is the deepest legal form.
  `t/oo/role/parametric/punning.t` is not — a third level buys navigability that a longer
  filename buys more cheaply.
- **Basenames are globally unique across the whole tree.** `t/regex/backtrack.t` and
  `t/vm/backtrack.t` may not coexist. This is enforced by `scripts/check-t-layout.sh`; see §5 for
  why it matters.

## 2. Categories

Sixteen top-level categories. They are a closed set: adding one is a deliberate change to this
document and to `scripts/check-t-layout.sh`, not something to do in passing.

| Directory | Holds |
| --- | --- |
| `lang/` | Surface syntax and the operator language: precedence, associativity, metaops, quoting, heredocs, interpolation, adverbs/colonpairs, sigils and twigils, literals, Pod, comments |
| `types/` | Individual value types and the conversions between them: `Int`/`Num`/`Rat`/`Complex`, `Str`/`Buf`/`Blob`, `Bool`, `Version`, `Date*`, enums, subsets, allomorphs, coercion, `Junction`, `Nil`/`Mu`/`Any`/`Cool` |
| `collections/` | `Array`, `Hash`, `List`, `Seq`, `Set`/`Bag`/`Mix`, `Range`, `Pair`, subscripts and slices, iteration and laziness, and the list-transforming routines (`map`, `grep`, `sort`, `reduce`, `zip`, `rotor`, …) |
| `control/` | Control flow and phasers: `for`/`while`/`loop`/`repeat`, `given`/`when`, `if`/`unless`/`with`, `gather`/`take`, loop-control verbs, the topic, `BEGIN`…`LAST` |
| `routines/` | Subs and their calling convention: signatures, parameters, captures, slurpies, placeholders, `proto`/`multi` dispatch, closures, `wrap`, `return`, `callsame`/`nextsame` |
| `oo/` | The object system: classes, roles, methods, attributes, accessors, traits, MRO and inheritance, mixins, submethods, construction (`new`/`BUILD`/`bless`), introspection and the MOP |
| `regex/` | The regex slang and the routines built on it: regex syntax, quantifiers, character classes, captures, anchors, backtracking and ratcheting, `Match`, `s///`, `tr///`, smartmatching against a regex |
| `grammar/` | `grammar`/`token`/`rule` declarators, action classes, `.parse`/`.subparse`, proto-regexes |
| `exceptions/` | `die`/`fail`/`throw`, `try`/`CATCH`/`CONTROL`, the `X::` hierarchy, `warn`, backtraces, exception introspection |
| `io/` | `IO::Path` and friends, file and directory operations, handles, sockets, `Proc`, the standard streams |
| `modules/` | `module`/`package`/`unit`, `use`/`need`/`require`, import and export, module search paths, `CompUnit`, precompilation, bundled batteries, zef/`mzef` distribution behaviour |
| `concurrency/` | `Supply`/`Supplier`, `Promise`, `start`, `await`, `react`/`whenever`, `Channel`, threads, `Lock`, atomics, `$*SCHEDULER` |
| `nativecall/` | `NativeCall`: `is native` subs, `CStruct`/`CArray`/`CPointer`/`CUnion`, REPR handling, native-type marshalling |
| `rakuast/` | The `RakuAST` compatibility surface (`src/rakuast/`) |
| `vm/` | Interpreter internals with no user-facing feature of their own: variable binding and assignment mechanics, scoping and the lexical environment, container/writeback coherence, `state`/`let`/`temp`, GC, JIT, bytecode and opcode behaviour, and ADR regression pins |
| `tooling/` | Things that are not the language: the CLI and its flags, `--dump-ast`, the LSP, `mzef`, exit codes, the `Test` module's own behaviour and the roast `Test::Util` helpers |

### Subcategories

A category nests one further level once it gets unwieldy. The soft cap is **~200 files**: past
that, `ls` stops being an index again and the category is split. For calibration, roast's own
largest directory is 70 files, so 200 is already generous. Ten of the sixteen categories are split
today; the largest directory in the tree is 193 files and the largest subcategory 167.

A subcategory needs no approval beyond being obvious from the category's contents (`t/oo/role/`,
`t/regex/subst/`), but it must be added in **two** places: `SUBRULES` in
`scripts/migrate-t-layout.py`, which is the source of truth, and `SUBCATEGORIES` in
`scripts/check-t-layout.sh`, which enforces it.

Do not create a subcategory holding three files. A flat category of 60 is fine.

## 3. Choosing a category

Categories overlap; that is unavoidable and not a problem as long as the choice is predictable.
Two tie-breakers, in order:

1. **Place by what the test would catch if it broke, not by the syntax it happens to use.** A test
   that writes `for @a { ... }` to prove that a closure captures the loop variable by reference is
   a `vm/` writeback test, not a `control/` loop test. A test that writes a class to prove
   `multi` dispatch picks the right candidate is `routines/`, not `oo/`.
2. **When two categories are genuinely equal, prefer the more specific one.** `grammar/` over
   `regex/`, `nativecall/` over `types/`, `rakuast/` over everything.

A test that pins a bug reported against one subsystem goes with that subsystem even if the
minimal repro no longer mentions it.

## 4. Filenames

**A migrated file keeps its basename exactly.** `t/regex-backtrack.t` becomes
`t/regex/regex-backtrack.t`, not `t/regex/backtrack.t`. The redundancy is deliberate: several
hundred `t/<name>.t` references in `docs/`, `news/` and code comments are prose, not links, so
nothing breaks them mechanically — keeping the basename keeps every one of them findable with a
single `git grep`, and makes the migration a pure `git mv` that reviewers can verify by name.

**A new file need not carry the category prefix.** `t/oo/role-punning.t` and `t/oo/punning.t`
are both fine; pick whichever reads better, subject to the global-uniqueness rule.

## 5. Why basenames stay unique

Several long-standing tools key on the basename rather than the path:

- `scripts/test-module-sweep.sh` copies every test into one flat work directory and indexes its
  two output files by basename.
- CI logs, `prove` output, and every historical `docs/` and `news/` entry name tests by basename
  in prose.

Making basenames unique is much cheaper than making all of that path-aware, and it preserves the
property that a bare test name in a five-year-old design doc still resolves. `make check-t-layout`
enforces it.

## 6. Support directories

`lib/`, `fixtures/` and `packages/` are not categories and hold no `.t` files. New module
fixtures belong in `t/lib/`; new data fixtures in `t/fixtures/`. The nine existing `lib-*` /
`*-lib` directories are grandfathered — do not add more, because a per-test top-level directory is
the same navigability problem this document exists to fix.

**Address a fixture from the repository root, never from the test file.** `prove`, `make test`
and CI all run from the repository root, so `use lib 't/lib'` and
`use lib 'roast/packages/Test-Helpers/lib'` work from any depth:

```raku
use lib 't/lib';                             # right: depth-independent
use lib $?FILE.IO.parent.add('lib').Str;     # wrong: assumes the file sits in t/
use lib $*PROGRAM.parent(2).add(...);        # wrong: the 2 is a depth
```

This is the one rule the migration actually cost something to learn. 129 test files addressed
their fixtures *relative to their own file* — `$?FILE.IO.parent.add('lib')`,
`$*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib")` — which silently resolved to the right
place only while every test sat directly in `t/`. Moving them one level down pointed those paths at
`t/<category>/lib`, and the tests failed to find `Test::Util` at all. They were rewritten to
root-relative literals, which is what the 100+ tests already saying `use lib 't/lib'` had been
doing all along.

A test that names **its own** path has the same problem. Prefer `$?FILE` itself; where a literal is
needed, match on the **basename**, which is unique and stable even if a file's category is re-cut
later.

## 7. Discovery

`prove` does not descend into subdirectories without `-r`. Every invocation of the suite therefore
passes it:

```
prove -r -e 'scripts/run-t-test.sh' t/
```

`-r` matches `*.t` only, so `t/fixtures/**/*.rakutest` and the 171 `t/lib/*.rakumod` fixtures stay
invisible to it. Verified against the flat tree before the migration: `prove -r --dry` and
`prove --dry` returned the same 3,938 files.

## 8. The migration

Done. 3,948 files moved on 2026-09-10, as `git mv` only — the diff is 3,949 renames with zero
insertions and zero deletions.

`scripts/migrate-t-layout.py` is the sweep, kept in the tree so the result stays reproducible: it
is the reason a ~4,000-file rename was reviewable at all, since anyone can re-run it and diff its
plan against `git ls-files` rather than taking the diff on trust. It decides placement in two
layers — an explicit `OVERRIDES` map of basename to category, consulted first, then an ordered
list of `RULES` where the first regex to match wins, followed by a per-category `SUBRULES` table
for the second level. A file matched by nothing at all is reported and the run refuses to apply,
so the tree can never end up half-placed.

`SUBRULES` is the source of truth for the subcategory list; `scripts/check-t-layout.sh` carries the
same list and the two must be kept in step.

Two things were deliberately **not** done:

- **Prose references in `docs/` and `news/` were not rewritten.** Basenames are preserved and
  unique, so a bare `t/<name>.t` in an old design doc still resolves by `git grep`; rewriting
  several hundred of them would have buried the move in noise.
- **`flaky-tests.txt` was not touched.** It is path-keyed rather than basename-keyed, so it would
  have needed rewriting — but it holds no `t/` entries, only `roast/` ones.
