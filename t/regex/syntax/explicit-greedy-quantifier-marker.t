use Test;

# `*!` / `+!` / `?!` are the explicit greedy markers (counterpart of the
# frugal `*?`). Found via Net::NetRC, whose `ws` token uses `\s*!`.

plan 8;

ok "aab" ~~ /^ a*! b/, '*! matches greedily';
ok "aab" ~~ /^ a+! a b/, '+! still backtracks into the quantifier';
nok "aab" ~~ /^ a?! b/, '?! takes at most one';
ok "b" ~~ /^ a*! b/, '*! matches zero times';

my grammar G {
    token TOP { ^ <ws> 'x' $ }
    token ws { <!ww> \s*! [ '#' \h*! <content> \n \s* ]* }
    token content { \N* }
}
ok G.parse("x"), 'grammar with *! in ws parses';
ok G.parse("  x"), 'leading whitespace';
ok G.parse("# c\nx"), 'comment then x';
is G.parse("# c\nx").chars, 5, 'whole input consumed';
