# More of rakudo's RakuAST::Deparse runs on mutsu

The second wave of what rakudo 2026.09's own `RakuAST::Deparse` needs from
mutsu (#14877):

- `nqp::getlexcaller` finds a dynamic a caller declared in a block, also when
  the call reaches the reader through a proto body's `{*}`. The deparser sets
  its dynamics up once in `proto method deparse` and recursed with them reset,
  so it wrote no statement delimiters.
- Multi dispatch ranks a RakuAST argument from the measured hierarchy without
  allocating its MRO for every candidate (4x on the deparser; the remaining
  cost of a `proto {*}` redispatch is #14868).
- More computed node methods, each after rakudo's definition:
  `Parameter.is-declared-optional`/`is-declared-required`/`outer-type`,
  `Package.parameterization`/`parsed-declarator`, `Term::Enum.name`,
  `Name.is-indirect-lookup`, `named-arg-name`/`named-arg-value` of pairs,
  `Phaser.original-blorst`, `IMPL-QAST-TYPE` of a conditional,
  `IMPL-TRAIT-NAME` of a trait, `Call::MetaMethod.args`, `Stub.name`,
  `Method.meta`, `Heredoc.has-variables`/`processors` and the slurpy
  placeholders' `lexical-name`.

On a quarter of roast, the share of top-level statements the vendored deparser
renders exactly as rakudo does rose from 11.9% to 55%.
