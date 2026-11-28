# An LTM alternation is ranked by its first character when that decides it

An LTM alternation (`[ a | b | c ]`, or a token whose body is one) is ranked
at every position where the matcher reaches it. Ranking means running the
branches' NFA to measure each branch's declarative prefix, and every branch is
measured even when only one can possibly begin at that position. YAMLish's
`inline` token has five branches: an alias, a flow map, a flow list, and
single- and double-quoted scalars. Ranking it at a `'` walked the
single-quoted branch's NFA through the whole quoted string, about 520 NFA
threads, on every scalar of a document.

Each root of a several-branch NFA now has a *gate*: the set of characters a
match of that branch can begin with. It is the node guard of #10710, but it
also looks through a `<rule>` call into the callee's body. When at most one
branch's gate admits the character at the ranking position, the ranking
needs no run. Every other branch fails at its first character, in the
measurement and in the match alike. The admitting branch goes first and the
rest keep their order, which is the order a run produces. A branch without a
gate (it can start with a code block, a lookaround, `<.ws>`, `:i` or an empty
match) leaves the ranking to the run. So does any live left-recursion
activation, because a run reads that activation's seed through a call
without testing a character.

One warm `load-yaml` of `bench-yaml-parse-big`'s document, `MUTSU_JIT=off`,
callgrind: see the PR for the before and after (#12917).
