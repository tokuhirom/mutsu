# `$*R` in a BEGIN phaser: a module can re-export another unit's EXPORT package

CompUnit::Util's `re-export` (and `re-export-everything`) call, from a BEGIN phaser, the
compile-time resolver `$*R`: `$*R.current-scope`, `nqp::getattr($*R, RakuAST::Resolver,
'$!export-package')` and `EXPORT.WHO.merge-symbols($handle.export-package)`. mutsu now binds `$*R`
for the extent of a BEGIN/CHECK phaser (undefined at run time, as in rakudo), answers
`current-scope`, `get-global` and the lookups with the host object ADR-15371 describes, and a merge
into the unit's own `EXPORT` stash publishes the incoming tags as the loading unit's exports.
`=:=` now treats a RakuAST node reached through a cell and the node itself as one object, which
`scope =:= $*CU` needs. The vendored CompUnit::Util `t/02-re-export` first case (`re-exports-stuff`)
loads and exports like rakudo; `steal-export-sub`, `re-exporthow`, `steal-globalish` and the
`set-*` / `get-*` calls need `RakuAST::Declaration::Import.new` and
`merge-generated-lexical-declaration` and stay on #15371.
