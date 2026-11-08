use Test;

# `Declaration::ResolvedConstant` wrapped in `Term::Declaration` is a term that
# evaluates to a compile-time value, as a CHECK-phase rewrite builds it.
plan 6;

my $c = RakuAST::Declaration::ResolvedConstant.new(:compile-time-value(7));
is $c.compile-time-value, 7, 'the value reads back';
is $c.^name, 'RakuAST::Declaration::ResolvedConstant', 'class name';

my $t = RakuAST::Term::Declaration.new($c);
is EVAL(RakuAST::StatementList.new(RakuAST::Statement::Expression.new(expression => $t))), 7,
    'a constant term evaluates to its value';

my class Greeter { method hi { "hi" } }
my $call = RakuAST::ApplyPostfix.new(
    operand => RakuAST::Term::Declaration.new(
        RakuAST::Declaration::ResolvedConstant.new(:compile-time-value(Greeter.new))),
    postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier("hi")));
is EVAL(RakuAST::StatementList.new(RakuAST::Statement::Expression.new(expression => $call))), "hi",
    'a method call on a constant instance';

is $t.raku.lines.elems, 5, 'raku prints the nested constructors';
ok RakuAST::Declaration::ResolvedConstant.^can('compile-time-value'), 'can compile-time-value';
