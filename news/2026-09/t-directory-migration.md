# `t/` is a nested tree now

The 3,948 flat `.t` files in `t/` moved into the sixteen subject categories defined by
[`docs/t-directory-layout.md`](../../docs/t-directory-layout.md), closing the second half of
[#7819](https://github.com/tokuhirom/mutsu/issues/7819). The design and the infrastructure landed
first, in a separate PR; this is the move itself.

The move itself is **3,949 renames with zero insertions and zero deletions**. It was not quite a
pure rename overall, though, and the reason is the most useful thing this change turned up.

## The one real cost: file-relative fixture paths

**129 test files addressed their fixtures relative to their own file rather than to the repository
root**, and that only ever worked because every test sat directly in `t/`:

```raku
use lib $?FILE.IO.parent.add('lib').Str;                          # -> t/lib, before
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
```

One level down, the first points at `t/<category>/lib` and the second at `t/` — so the tests could
not find `Test::Util` at all and aborted before running a single assertion. They now use
root-relative literals (`use lib 't/lib'`), which is depth-independent because `prove`, `make test`
and CI all run from the repository root, and is exactly what the 100+ tests already saying
`use lib 't/lib'` had been doing all along.

Four more files named a *path* rather than loading one: three asserted on their **own** filename
(`$?FILE`, `callframe.file`) and now match on the basename, which stays unique and stable even if a
category is re-cut later; and one Rust integration test,
`tests/proto_method_body_compiled_once.rs`, executes a specific `t/` file by path. That last one was
caught instantly by `cargo test` — which, because it aborts `make test` before `prove` runs, is also
why the first full-suite attempt never reached the TAP files at all.

`docs/t-directory-layout.md` now states the rule directly: address a fixture from the repository
root, never from the test file.

## What it looks like

Ten of the sixteen categories carry a second level, because a category past roughly 200 files is
no more browsable than the flat directory was. For calibration, roast's own largest directory is
70 files. After the split the largest directory in the tree is `t/nativecall/` at 193 and the
largest subcategory `t/routines/signature/` at 167:

```
t/collections/{array,hash,lazy-seq,range-pair,set-bag-mix,subscript,transform}/
t/concurrency/{promise,supply,thread-lock}/
t/lang/{adverbs,operators,parsing,quoting}/
t/modules/{batteries,compunit,import-export}/
t/oo/{attribute,class,construct,method,mop,role,trait}/
t/regex/{match,subst,syntax}/
t/routines/{call,closure,dispatch,signature}/
t/types/{coercion,enum-subset,numeric,string,temporal}/
t/vm/{binding,codegen,frames,scope,writeback}/
t/{control,exceptions,grammar,io,nativecall,rakuast,tooling}/
```

## Why a script rather than a hand-sorted list

`scripts/migrate-t-layout.py` performed the sweep and stays in the tree. A ~4,000-file rename is
unreviewable as a diff; it is reviewable as a *rule set* plus its output, because anyone can re-run
the script and diff its plan against `git ls-files`. It places a file by consulting an explicit
`OVERRIDES` map first, then an ordered `RULES` list (first regex wins, most specific subject
first), then a per-category `SUBRULES` table for the second level. A file that matches nothing is
reported and the run refuses to apply, so the tree cannot end up half-placed. Six files needed a
hand-written override; the rules placed the other 3,942.

## What was deliberately left alone

**Filenames.** A migrated file keeps its basename exactly: `t/regex-backtrack.t` became
`t/regex/syntax/regex-backtrack.t`, not `.../backtrack.t`. Several hundred `t/<name>.t` references
in `docs/`, `news/` and code comments are prose rather than links, so nothing rewrites them
mechanically — an unchanged, unique basename keeps every one of them findable with a single
`git grep`, and let reviewers check the move by name. `make check-t-layout` enforces that
basenames stay globally unique, which is also what keeps `scripts/test-module-sweep.sh`'s flat
work directory working.

**Prose references.** Not rewritten, for the reason above: several hundred edits would have buried
the move in noise.

**`flaky-tests.txt`.** Path-keyed rather than basename-keyed, so it would have needed rewriting —
but it holds no `t/` entries, only `roast/` ones.

## The guard is fully armed

`scripts/check-t-layout.sh` was already running as a `make test` prerequisite and a CI step,
enforcing known categories, the two-level depth cap, no `.t` under a support directory, and unique
basenames. Its last rule — **no `.t` at `t/` top level** — was held behind a flag until the move;
that flag is gone and the rule is live, so the flat layout cannot creep back one file at a time.
