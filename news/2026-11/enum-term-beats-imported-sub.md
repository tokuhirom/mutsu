# A bare enum member now wins over a same-named imported sub

A module that declares `enum UnitName (minutes => 60)` and also exports `sub minutes(Numeric() $n)`
(the TimeUnit distribution) made the importer's argument-less `minutes` call the sub, dying with
"Too few positionals passed". The bareword call prefix for block-scoped imports now declines when the
scope holds an enum member of that name, matching rakudo, which resolves the term before the routine.
`minutes(30)` still calls the sub. TimeUnit's `t/01-usage.rakutest` passes 20/20, like raku.
