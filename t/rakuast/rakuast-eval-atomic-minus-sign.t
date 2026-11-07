use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# `⚛−=` spelled with U+2212 MINUS SIGN is the atomic subtract-assign: the AST
# keeps the spelling, and EVAL($ast) runs it as `⚛-=`. Checked against rakudo.

plan 4;

my $code = Q[my atomicint $i = 5; $i ⚛−= 2; $i];
my $ast = $code.AST;
ok $ast.raku.contains('Infix.new("⚛−=")'), 'the AST keeps the U+2212 spelling';
is EVAL($code), 3, 'source: subtracts';
is EVAL($ast), 3, 'EVAL($ast): subtracts';
is EVAL(Q[my atomicint $j = 9; $j ⚛-= 4; $j].AST), 5, 'the ASCII spelling still works';
