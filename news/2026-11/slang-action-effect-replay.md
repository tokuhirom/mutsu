# A slang action's effects outside the metamodel reach the importing unit

A slang production's token and action run in the slang host, a separate interpreter, so anything an
action did besides attaching a node stayed there: Qwiratry's `mold` pushed its `Mold` onto a
module-level registry the unit never saw, and `transformer Foo is Transformer { mold TOP do { ... } }`
composed a class with no molds. The host now records the slang's own action calls while a production
runs, and the statement it returns carries the record: the unit calls the same action methods over
the same Match nodes (with the `:my` dynamics their rules declared, `$*BLOCK`), once per production,
so the effects happen in the interpreter that runs the program (#17243, ADR-17243 Proposed).
Qwiratry's `t/unit/mold-slang.rakutest` passes, as under `raku`. Also fixed: a keyword (`self`) in an
`EVAL`'d snippet is never reported undeclared because a module exports a term of that name.
