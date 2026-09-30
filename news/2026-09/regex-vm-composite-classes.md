# The compiled regex engine runs composite character classes

ADR-0135's compiled regex engine (Slice A, #10251) now compiles composite character classes,
such as `<+alpha -[x]>`, `<[a..z] - [c]>` and `<-vowel>`. They were the largest Slice A decline
reason left: 119 patterns over `t/` and the roast whitelist.

A composite class is one more one-grapheme atom, tested by the walk's own
`match_consuming_atom`. Classes with a named item are the one subtlety. When the built-in class
rejects a character, the walk falls back to a grammar token of that name, and that token depends
on the package and reads the real subject. The compiled engine's ASCII fast path probes each
atom once per program against a scratch string, so such atoms skip the probe table and always
call the full function.

While checking this against rakudo, we found a difference that walk and engine share. In a
grammar that overrides `alpha`, `<+alpha>` still accepts the built-in alpha characters, where
rakudo uses the grammar's `alpha` alone. That is filed as #10305.

`MUTSU_VM_STATS` also stops lumping unrelated atoms together as `other-atom`. It now reports
`ws-rule`, `isolated-group` (`<$rx>`), `interpolation`, `goal-match` and `conjunction`
separately.
