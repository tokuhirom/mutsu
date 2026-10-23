# RakuAST frontend: 13 whitelisted roast files that ran differently now match

Running every whitelisted roast file under `MUTSU_RAKUAST=1` and comparing with the ordinary frontend found 13 that failed only
through the RakuAST round trip. They shared a dozen small causes, each now fixed: `not(...)` as a call, a chain ending in a regex
smartmatch, negated infixes (`!=:=`, `* !< 3`), a `my \term` initializer reading `e`, a block-scoped `constant True`, `A(Any)`,
`Seq.from-loop(:label(...))`, a single-quoted regex atom containing `&`, `<&Pkg::name>`, the written order of `is` / `does` /
`hides`, a placeholder under an explicit signature, and the sink warning for `<42i>`. See ADR-10723 §6.108 and
`t/rakuast/rakuast-roast-tap-cohort.t`.
