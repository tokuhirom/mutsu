use experimental :rakuast;
use Test;

# A user postfix operator applied as an operator is an `ApplyPostfix` in
# RakuAST, as on rakudo, not a call of the operator routine. Red's
# `red-compile` reads `.number++` this way while Red's own `postfix:<++>` is
# imported (Red t/091-rakuast-compiler).

plan 11;

my $ast = Q[sub postfix:<!>($n) { [*] 1..$n }; 5!].AST;
my $apply = $ast.statements[*-1].expression;
isa-ok $apply, RakuAST::ApplyPostfix, 'a user postfix is an operator application';
isa-ok $apply.postfix, RakuAST::Postfix, 'its postfix is a Postfix node';
is $apply.postfix.operator, '!', 'the operator symbol';
is $apply.DEPARSE, '5!', 'it deparses as written';
is EVAL($ast), 120, 'the round trip calls the user routine';

$ast = Q[multi postfix:<++>(Str $x is rw) { $x = 'inc' }; my $s = 'a'; $s++; $s].AST;
isa-ok $ast.statements[2].expression, RakuAST::ApplyPostfix,
    '`$s++` with a user postfix:<++> in scope is an application';
is EVAL($ast), 'inc', 'the round trip still dispatches to the user candidate';

sub postfix:<!>($n) { [*] 1..$n }
is EVAL(RakuAST::ApplyPostfix.new(
    operand => RakuAST::IntLiteral.new(4),
    postfix => RakuAST::Postfix.new(operator => '!'))), 24,
    'a hand-built Postfix of a user operator calls it';

my $i = 1;
EVAL RakuAST::ApplyPostfix.new(
    operand => RakuAST::Var::Lexical.new('$i'),
    postfix => RakuAST::Postfix.new(operator => '++'));
is $i, 2, 'a hand-built core ++ still increments';

is (1, 2, 3).map(*!).List, (1, 2, 6), 'a whatever-curried user postfix still works';
is (3!)!, 720, 'nested user postfix applications still work';
