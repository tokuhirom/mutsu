# A block-scoped hook import keeps its multi family inside the block

A `use M 'x'` inside a block, where `M` has a `sub EXPORT` that hands out one
of its own multi families (`UNIT::{"&$_"}:p`), loads `M` at parse time
(ADR-17558) and installs the hook's map where the `use` stands. The import
granted the module's scoped family to the whole importing unit
(`operator_import_units`), and nothing took that grant back when the block
ended, so the family stayed callable after the block and from an `EVAL`.
rakudo, like a `use` whose argument has to be evaluated, keeps it to the
block.

The grant is now logged on the block's import scope as an import write
(`ImportWrite::FamilyGrant`), and the scope's exit revokes it with the
registry writes it already undid (#18100).

With the leak gone, `use M <x y>` takes the same parse-time path as
`use M 'x'`: the angle-bracket words are literal `use` arguments, so a hook
module is loaded while the unit parses and the operators its load binds into
`EXPORT::MANDATORY` are known to the parser (`use annotations <declare direct
class>` in the annotations distribution, #18091).
