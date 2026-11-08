use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# The imaginary-unit postfix over a term (`(1 + 2)i`, `$x\i`, `NaN\i`) is
# ApplyPostfix(operand, Postfix "i") with the parentheses dropped, deparses
# back, and evaluates to the same Complex as the source. Checked against rakudo.

plan 8;

for Q[(1 + 2)i], Q[my $x = 2; $x\i], Q[(NaN)i] -> $code {
    my $ast = $code.AST;
    ok $ast.raku.contains('RakuAST::Postfix.new(') && $ast.raku.contains('operator => "i"'), "ApplyPostfix with Postfix i: $code";
    is EVAL($ast).raku, EVAL($code).raku, "EVAL(\$ast) agrees with the source: $code";
}

is Q[(1 + 2)i].AST.DEPARSE.trim, '(1 + 2)i', 'DEPARSE keeps the parentheses it needs';
is Q[NaN\i].AST.DEPARSE.trim, 'NaN\i', 'and the backslash form';
