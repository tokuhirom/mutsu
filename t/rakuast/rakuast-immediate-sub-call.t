use v6;
use experimental :rakuast;
use Test;

# `sub foo($n) { … }(21)` declares `foo` and calls it on the spot. rakudo's
# tree keeps that source form: an `ApplyPostfix` whose operand is the `Sub`
# and whose postfix is a `Call::Term` (measured on 2026.09).

plan 9;

my $statement = Q[sub bar($n) { $n + 1 }(10)].AST.statements[0];
my $call = $statement.expression;
isa-ok $call, RakuAST::ApplyPostfix, 'an immediately called sub is a postfix application';
isa-ok $call.operand, RakuAST::Sub, 'its operand is the routine declaration';
is $call.operand.name.raku, 'RakuAST::Name.from-identifier("bar")', 'the routine keeps its name';
isa-ok $call.postfix, RakuAST::Call::Term, 'the postfix is a term call';
is $call.postfix.args.args.elems, 1, 'with the written argument';

my $do = Q[my $r = do sub foo($n) { $n * 2 }(21)].AST.statements[0].expression;
isa-ok $do.initializer.expression, RakuAST::StatementPrefix::Do,
    'a `do` over it is a statement prefix';

is EVAL(Q[
    my @log;
    sub bar($n) { @log.push($n); $n + 1 }(10);
    @log.push(bar(5));
    @log.join(',')
].AST), '10,5,6', 'the routine runs once in place and stays declared';

is EVAL(Q[my $r = do sub foo($n) { $n * 2 }(21); $r].AST), 42,
    'the call value is the value of a `do`';

is EVAL(Q[sub zero { 'z' }()].AST), 'z', 'an argument-less immediate call round-trips';
