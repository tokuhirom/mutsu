# By-name local-slot lookups no longer scan the frame's locals

Several opcodes still found a variable by comparing its name against every
local of the running chunk. Each execution therefore cost O(L), L = the
frame's locals. `find_local_slot` already answered from the chunk's lazily
built name index (#9170). This change moves the remaining scans onto that
index (#9171):

- `SetVarDynamic` (every `my` declaration) tested whether the name was a
  `state` variable by formatting `::<name>@` and scanning every
  `state_locals` key. It did this twice per declaration while the
  cross-thread shared store was active. The index now carries the set of
  `state` names, so the test is one probe, and a sub with K `my`
  declarations costs O(K) per call. `scripts/vm-complexity-check.sh`
  measures K = 200 → 400 at ratio 2.18 (expected 2).
- `GetOuterVar`'s inline nested-block path, the `MarkSigillessBind`
  fallback without a paired source verdict, and the `our`-alias sync after
  a symbolic store (`$::('Pkg::v') = ...`) now probe the index as well. The
  sync uses a new `our_locals`-by-name table.
- `DoGivenExpr`, `UndefineAggregate`, `RegisterPackage`,
  `RegisterPackageMy`, `TopicDotAssign`, `SymbolicDerefStore` and
  `IndirectTypeLookupStore` already reached the index through
  `find_local_slot`. Only their stale `O(L)` cost annotations changed. A
  `do given` at L = 4000 and 8000 now times the same.

The pseudo-stash reads (`Pkg::<$x>`, `OUTER::`, `DYNAMIC::`, ...) and
`PackageScope` still rebuild their view from the whole env. #9171 stays open
for them.
