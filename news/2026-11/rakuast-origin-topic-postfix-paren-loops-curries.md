# RakuAST origins of topic postfixes, loops in parentheses and curried calls

`.AST` now places, as rakudo 2026.11 does:

- a postfix operator on the topic (`.++`, `.--`), from after its dot;
- a loop statement inside parentheses (`(repeat { $n++ } while $n < 3)`,
  `(loop { last })`), which the parser left unplaced. Its text is parsed
  again as a fragment and the ranges are copied over;
- a call on a parenthesized curry (`(* + *)(3, 4)`): the curry inside the
  parentheses, with its operator between two primed `*`, and the
  `Call::Term`, which starts after the closing parenthesis (#15780).
