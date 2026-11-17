# A relative `use lib` path is made absolute, so a module loads once

Rakudo turns `use lib 'lib'` into the absolute repository `file#$*CWD/lib`. mutsu kept the
relative spelling. A distribution's tests are usually run with `-I /abs/dist/lib` **and**
start with `use lib 'lib'`, so the one directory was on the chain twice, under two names:

- a parse-time probe or a slang activation, which searches the command-line chain, loaded
  a module as `/abs/dist/lib/M.rakumod`;
- the unit itself loaded it again as `lib/M.rakumod`;
- the two loads compiled in different contexts and overwrote each other's precompiled entry
  on **every** run ("context changed"), so the cache never hit.

`use lib`, `-I`/`MUTSULIB` and the parser's own `use lib` folding now all make a plain
relative path absolute (`$*CWD` at runtime, the process directory at parse time). The
directory is then one repository, each module file is loaded and cached once, and the
precompiled entries hit from the second run on.

Qwiratry (#17690), release build, warm:
- `t/unit/aggregate-body-whole-tree.rakutest`: 7.3 s → 2.6 s (rakudo 0.7 s);
- `t/integration/rakuast-tree-navigator.rakutest`: 21.8 s → 10.8 s (rakudo 4.1 s).
