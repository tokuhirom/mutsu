# A block's import of `our &name is export` writes no `&name` env alias

`import_module` bound an exported code variable in the block's lexical import
set and then also wrote the `&name` env alias (recorded for the scope exit's
rollback). When the set binds the value, the alias is no longer written: a bare
call, `&name`, `MY::` and a by-name lookup all read the set. A top-level import,
which has no set, keeps the alias.
