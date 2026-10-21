# A package-less module's `my constant` no longer stays in the importer's env

A module file without a `unit` declarator ran its body in the loading program's
env and left every `my constant` behind under its term key, so the program's
`EVAL 'HUFF'` saw a constant Rakudo keeps lexical to the compunit, and every copy
of a program frame carried the entries (#7817). The load now drops the file-scope
`my constant` keys from the loader's env; the value stays in the module's scope
tables, so the module's own routines still read it (#14330).
