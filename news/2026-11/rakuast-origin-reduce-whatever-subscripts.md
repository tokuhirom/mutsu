# RakuAST origins: reductions, subscripts of `*`, angle-quoted numbers

More nodes carry the `.origin` rakudo 2026.11 gives them (#15780):

- a reduction's operator and arguments (`[+] 1, 2`, `[\+] ...`, `[+](1, 2)`);
- the subscript and the postfix operator of a curried `*` (`*{}`, `*[0]`,
  `*<a>`, `*++`), whose `*` itself has no origin;
- an angle-quoted number (`<1/2>`, `<1+2i>`), whose range is the text inside
  its brackets.

`RakuAST::Pragma` (`use lib "x"`) no longer visits its argument in
`visit-children`, matching rakudo, which visits none of its parts.
