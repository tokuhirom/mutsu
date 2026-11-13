# A module routine names its own `my package`'s types after the module loaded

A module's `my package Enums { our enum Status ... }` is stored under its
declaration's own name (`Enums\0<id>`), and inside the module the bare `Enums`
is bound to it. A routine of that module naming `Enums::Status` and called later
from the program ran without that binding, so the qualified name resolved
against `GLOBAL::Enums` and died with "Could not find symbol '&Status'". The
head of a qualified bareword now also resolves through the lexical types the
routine's own compunit declared, as rakudo resolves the name at compile time in
the routine's lexical scope. The program itself still cannot name the module's
lexical package. KeyNub::LicDongle's `t/01-basic.rakutest` now passes 7/7 like
rakudo.
