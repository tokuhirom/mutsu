use Test;
use experimental :rakuast;

plan 24;

# An operator written in brackets keeps its `BracketedInfix` node, inside the
# `!` / `R` of a negated or reversed one, as rakudo does (#18504).

sub infix-of(Str $code) { $code.AST.statements[*-1].expression.infix }

my @cases =
    '1 [+] 2',      'RakuAST::BracketedInfix',  3,
    '1 [eq] 2',     'RakuAST::BracketedInfix',  False,
    '1 [cmp] 2',    'RakuAST::BracketedInfix',  Less,
    '1 [R-] 2',     'RakuAST::BracketedInfix',  1,
    '1 [[+]] 2',    'RakuAST::BracketedInfix',  3,
    '1 ![==] 2',    'RakuAST::MetaInfix::Negate', True,
    '1 R[+] 2',     'RakuAST::MetaInfix::Reverse', 3,
    '1 + 2',        'RakuAST::Infix',           3;

for @cases -> $code, $class, $value {
    is infix-of($code).^name, $class, "$code: the infix";
    is $code.AST.DEPARSE.trim, $code, "$code: DEPARSE";
    is-deeply $code.AST.EVAL, $value, "$code: EVAL";
}
