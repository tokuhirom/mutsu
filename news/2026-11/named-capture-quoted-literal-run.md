# `$<key>='run'` captures the whole literal

An aliased quoted literal lowered to one token per character and only the first token
carried the alias, so `"run x" ~~ /$<key>='run' .*/` gave `key` = `r`. A multi-token run is now
wrapped in one group that carries the alias. Jupyter::Kernel's `Magic::Grammar` (`token args
{ $<key>='run' ... }`) relies on it (`t/06-magic.rakutest` test 49).
