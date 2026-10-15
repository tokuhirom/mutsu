use v6;
use experimental :rakuast;
use Test;

plan 5;

my $ast = Q[gather { take 2 andthen 3 }].AST;
ok $ast.raku.contains('RakuAST::ApplyInfix.new('),
    'take with a loose word-logical is one infix expression';
ok $ast.raku.contains('RakuAST::Call::Name::WithoutParentheses.new('),
    'the left operand remains the bare take call';

my @log;
my @taken = EVAL(Q[gather { take (do { @log.push('X'); 2 }) andthen @log.push('Y') }].AST);
is-deeply @taken, [2], 'andthen preserves the taken value';
is-deeply @log, ['X', 'Y'], 'the value runs once before the andthen tail';

@log = ();
my @zero = EVAL(Q[gather { take (do { @log.push('X'); 0 }) and @log.push('Y') }].AST);
is-deeply @log, ['X'], 'and short-circuits after taking a false value';
