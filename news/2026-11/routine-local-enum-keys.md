# An enum declared inside a routine is that routine's lexical

`method m { enum Mode <Normal Yaml>; my Mode $m = Normal }` failed in two ways.
The type-qualifying pass counted the enum as an `our` type of the enclosing
class and rewrote `Mode` to `A::Mode`, which the run time never registers; and
the executing finalizer treated `Normal` as the setting's term whenever an
outer list redeclared the same key later (`enum Volume (:Normal(0))`), even
after the routine had declared it itself.

Enums and subsets declared in a routine body are now left alone by the
qualifier, and a term declared earlier in an inner list shadows an outer
list's later redeclaration. Found with the TAP distribution, whose two test
files now match rakudo (6 and 61 assertions).
