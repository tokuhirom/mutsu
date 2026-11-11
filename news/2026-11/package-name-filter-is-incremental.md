# Loading many modules no longer rebuilds the package name filters

The `package -> name -> value` tables behind module-scope lexicals, imported
lexical names and package type aliases (`PackageKeyed`) answer "is this name
in any package at all?" from a cached filter of every key. Any write dropped
the filter and the next read rebuilt it from every entry of every package. A
module load alternates the two -- each import binds names, each export lookup
asks the filter -- so loading was O(imports x table size). The package type
alias table was also cloned whole on every block-scoped `use` while the import
replay probe held a snapshot of it, and the clone then rebuilt its filter from
scratch.

Now each package's map is a `NameMap` shared between a table and its
copy-on-write clones (`Arc`, copied only when one side writes that package),
carrying the id of the table whose filter already holds its keys. A write
marks that one map as not indexed; the next query extends the previous filter
with the unindexed maps only, and a clone starts from its source's filter. A
name removed from a map may stay in the filter, which only costs the chain walk
the filter would otherwise have skipped.

`use Selkie::UI` (137 modules, warm precompilation cache) went from 33 s to 9 s,
and each of Selkie::UI's seven test files from ~47 s to ~9 s (rakudo: ~3 s with
its precompiled modules); all seven still match rakudo.
