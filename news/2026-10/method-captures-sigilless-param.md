# A method reads a sigilless parameter of the routine that declares its type

`sub mk(Mu \t) { role { method of { t } } }; (Any.new but mk(Int)).of.^name`
answered `Str` where Rakudo answers `Int`: the bare `t` in the method read as
the string "t". The `$t` spelling of the same capture worked (#11804).

A method body is compiled by its own `Compiler::new()`, which deliberately
inherits almost nothing from the compiler of the routine around it
(`compile_method_body`). Only the `&`-lexicals were handed down, so the method
never learned that `t` names a sigilless binding of an enclosing routine, compiled
it as a bareword lookup, and found nothing in the method's frame. The sigilless
names of the declaring scope (`\t` parameters and `my \x`, with the ones that
scope itself inherited) are now handed down too. The method then compiles `t` as
the by-name read a nested closure already uses, which is the read the free-variable
analysis records and the declaring frame captures. The same fix covers a class
declared inside the routine, nested routines (every enclosing level is visible)
and a `^parameterize` meta-method that mixes in a role.
