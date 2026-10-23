# RakuAST frontend: core infix adverbs and dunder variable traits

Two more files leave the `MUTSU_RAKUAST=1` refusal list. `EVAL` of an `ApplyInfix` with adverbs on a core operator (`1 + 2 :adverb`)
now lowers to the operator call, which the core operator rejects as on the ordinary frontend, and a user variable trait named like
a parser marker (`my $x is __x`) round-trips as a `Trait::Is` of that name. The ratchet list also gains the 46 files the survey
found already passing. See ADR-10723 §6.107.
