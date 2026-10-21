# A top-level `require` binds its imports lexically too

`require M <&f $x T K>` at a unit's top level now compiles the same way as one
in a block (ADR-8751, #14140): the listed symbols are lexicals of the unit,
bound from the values the statement looks up, instead of a `GLOBAL::f` routine
alias and env aliases written at run time. Every `require` with a module name
and a literal import list goes this way; only a `require` whose module or list
is computed keeps the run-time aliases.

The lookup is now a pure `required_symbol` that writes nothing; the run-time
path applies the env and registry writes on top of it. A sigil-less name the
list carries is also a declared type of the unit, as Rakudo stubs it at compile
time, so `sub f(T $x)` and `my T $o` accept it before the `require` runs.
