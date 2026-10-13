# RakuAST: a declaration followed by a postfix

`my @a.[2] = 42`, `my %h.<k> = 1`, `my @a.push(1)` and `my Int $x.say` now
convert to rakudo 2026.09's `ApplyPostfix` over the `VarDeclaration::Simple`
and `EVAL` back, so they round-trip under `MUTSU_RAKUAST=1`. The round-trip
frontend ratchet grows by 3 files.
