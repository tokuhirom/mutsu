# A built-in operator site remembers that no `use` binds it

Once any module in the process imports user candidates of a built-in operator for
its own blocks (Cro does), every built-in operator opcode compiled under an import
chain asked the chain on every execution whether one of its import sets binds that
operator (`exec_lex_import_builtin_op_site` -> `lexical_import_operator_binding`,
two set lookups and a name scan): about 1.8k Ir per executed operator in a plain
loop, even though the answer changes only when an import set is rewritten.

The site now remembers a miss for one import-set generation (the counter the
imported-routine call memo of #14371 uses, `ModuleState::lexical_import_gen`), keyed
by the import chain, in a second per-constant table of `LexImportSiteCaches`. A
lookup made under a built-in operator fallback answers differently, so it is neither
read nor recorded. `$s += %h<a>` in a loop, with `use Cro::HTTP2::RequestParser`
loaded (profiling build, callgrind with collection switched on around the loop only):
6,081 -> 5,350 Ir per iteration; without the module 4,076 -> 4,076.
