# The parser's module export scan is cached on disk

A `use Foo;` made mutsu read `Foo`'s source twice. The run-time half consults
the precompilation cache and skips its parse on a hit; the parser half — which
scans the module to learn what it exports, so the importing unit can parse calls
to those exports — had no cache at all and re-parsed from source in every
process. On a warm `use YAMLish; say "ok"` it was the larger half:
`find_and_scan_module` alone accounted for 209 M of the 334 M instructions the
run cost ([#8095](https://github.com/tokuhirom/mutsu/issues/8095)).

The scan now persists to `~/.cache/mutsu/modscan/`, next to the precompilation
cache and keyed the same way (canonical source path, mtime + content hash, and
the interpreter version stamp that embeds the executable's mtime, so a rebuilt
mutsu never reuses an older build's entries).

Measured with callgrind on `use YAMLish; say "ok"`, release build, precompilation
cache warm in both rows:

| module scan cache | Ir |
| --- | --- |
| cold (previous behaviour) | 271,924,883 |
| warm | 124,158,716 |

## The invalidation question the precompilation cache does not have

A scan result deliberately carries **transitive** names: the qualified type
names, enum values and sigilless constant terms that reached the module from the
modules *it* `use`s, because an importer can see those too. So the result is a
function of the dependency sources as well, and a content hash of the module's
own source would happily serve a stale answer after a dependency was edited.

Each entry therefore records every module its scan resolved, as a `ScanDep`: the
module name as written, the file it resolved to (or `None` when it resolved to
nothing the parser could scan — a name that resolves to nothing today and to a
file tomorrow changes the answer too), and that file's mtime + hash. The list is
the transitive closure — a child scan's deps are merged into its parent's — so
validating one entry validates the whole subtree without loading the children's
entries. Validation also **re-resolves** each dependency's module name through
the parser's current search path and requires it to land on the same file, which
is what keeps the cache honest across a changed `-I` or `use lib`.

## What a hit has to replay, and what it may drop

A hit runs no nested parse, so anything the scan used to leave behind as a side
effect had to become part of the entry. One such effect was found and captured:
inline `module Foo { ... is export }` tables. The nested parse registers those in
the process-wide inline-export table, which the scan does not restore, so an
`import Foo;` in the importing file finds them purely because the scan ran. They
are now recorded in the entry and replayed on every importer.

Parse warnings are deliberately *not* persisted. They are re-raised — and
deduplicated by `(file, message)` — when the runtime actually loads the module,
which always happens for a `use`; the in-process scan memo has behaved this way
for every `use` after the first since it was added.

Two situations skip the disk cache entirely rather than risk an unsound entry:
a module whose source says `no precompilation;`, and a scan running underneath
an EVAL that has preseeded the parser with names from its calling unit (the
result is then not the pure function of the module sources that the key assumes).
`MUTSU_PRECOMP=0` and `--no-precomp` turn it off with the rest of the caching.

## Tests

`src/scan_cache.rs` unit-tests the invalidation rules directly: an edited
module, an edited dependency, a dependency that now resolves to a different
file, and a dependency that was absent and is now present must all miss.
`t/modules/compunit/module-scan-precomp-cache.t` pins the end-to-end behaviour
across separate processes — which is the only place the disk cache exists, since
within one process the scan is memoized in a thread-local and the bug class
cannot appear. It probes with an *exported operator*, so a lost or stale export
list is a hard compile failure rather than a subtle difference.
