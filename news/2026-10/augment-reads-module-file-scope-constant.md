# A method `augment` installs reads its module file's `my constant`s

A module file without `unit` that declares `my constant @list = ...` and then
`augment class C { method list { @list } }` answered `Nil` when the method was
called after the load: the augment body is compiled from the raw AST at run
time, so it cannot capture the file's slot, and the constant had been dropped
from the loader's env at the end of the load. The file's `my constant`s are
now also kept in the compunit's unit-lexical store, the one a module routine
reads its file-scope lexicals from (a `unit` file already did this). The
`Locale::Dates` distribution's `t/01-basic.rakutest` now matches rakudo, 15/15.
