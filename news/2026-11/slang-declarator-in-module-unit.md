# A slang's package declarator works inside a module that `use`s the slang

`unit module X; use EEL; our event Flow { ... }` declared `Flow` as a plain
class: the slang's `define_slang`, run from EEL's `EXPORT` while `X` itself was
loading, was skipped wholesale so that the slang would not leak into `X`'s
importer — and with it the record of which metaclass `event` builds with, which
`X`'s own code needs when it registers `Flow` at run time. The declarators are
now recorded in that case too (the parser rules still are not), so `Flow` is
built with `MetamodelX::Event` exactly as in a script. EEL's precompiled-module
tests (`t/06-runner.t`, `t/07-goal.t`, `t/08-supply-match.t`) use this shape.
