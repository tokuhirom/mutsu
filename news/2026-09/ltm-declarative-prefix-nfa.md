# LTM ranking runs a compiled NFA

A `|` alternation ranks its branches by the length of their declarative prefix
(ADR-0022). mutsu used to measure that by running the backtracking matcher in a
"declarative" mode, and so paid for capture stores, candidate continuations and
an allocation per set of ends, just to get one number. On the #9617 grammar,
`regex A { '{' [ <A> | . ]*? '}' }`, that made a parse about 45 times slower
than Rakudo, even after the earlier fixes had brought it from exponential time
down to Rakudo's own quadratic order.

Now each branch is compiled once, the way Rakudo does it, into an NFA of its
declarative prefix (ADR-0125):

- the NFA inlines the subrules the branch calls;
- a recursive call is cut into a fate;
- every atom the walker treats as a fate (`<.ws>`, a code block, `** {code}`,
  ...) is one here too;
- a ranking runs the NFA over the subject;
- single atoms are still answered by the matcher's own atom functions, so the
  NFA holds no second copy of what a literal or a character class matches;
- the NFA is cached on the pattern per package and token generation.

Protos, subrules with arguments, lexical `<&re>` calls, `:m`, and a live
left-recursion activation are still measured by the walker.

On the #9617 repro (release build), the `regex` form drops from 0.67 s to
0.031 s at n=256 and from 1.02 s before #9625. At n=1024 mutsu takes 0.35 s
against Rakudo's 0.19 s.

`MUTSU_LTM_NFA_VERIFY=1` measures every NFA ranking with the walker as well and
prints each difference. That run found one real walker bug, fixed here: a
separated `** {code}` quantifier (`<x> ** {$n} % ':'`) evaluated its count
during a measurement instead of ending the prefix, which Rakudo does (and
ADR-0009 requires).
