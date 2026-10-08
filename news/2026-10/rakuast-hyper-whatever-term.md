# RakuAST: the `**` term lowers back and can be built by hand

`.AST` already rendered a `**` term as `RakuAST::Term::HyperWhatever`, but the
node refused to lower back (`lowering RakuAST::Term::HyperWhatever`) and
`RakuAST::Term::HyperWhatever.new` was not a known constructor. Both now work,
so a unit that uses `**` (a lazy slice, a `...` end, a smartmatch) crosses the
round-trip frontend. 6 more `t/` files pass under `MUTSU_RAKUAST=1`, plus the
files that already passed on `main` and were not yet in the ratchet.
