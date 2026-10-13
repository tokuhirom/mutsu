use v6;
use Test;
use experimental :rakuast;

# `$.n OP= EXPR` as a statement is, on rakudo 2026.09, an ApplyInfix whose left
# operand is the Var::Attribute::Public `$.n` and whose infix is a
# MetaInfix::Assign -- the same node the parenthesized expression form builds.
plan 10;

sub method-statement(Str $source) {
    my $method = $source.AST.statements[0].expression.body.body.statement-list.statements[1];
    $method.expression.body.statement-list.statements[0].expression
}

# --- read direction ---------------------------------------------------------

my $add = method-statement(Q[class ReadB { has $.n is rw; method m { $.n += 100 } }]);
is $add.^name, 'RakuAST::ApplyInfix', 'a statement `$.n += 100` is an ApplyInfix';
is $add.left.^name, 'RakuAST::Var::Attribute::Public', 'over the public attribute';
is $add.left.name, '$.n', 'whose name is the whole spelling';
is $add.infix.^name, 'RakuAST::MetaInfix::Assign', 'with the assignment metaoperator';
is $add.infix.infix.operator, '+', 'over the plain operator';

my $cat = method-statement(Q[class ReadC { has $.s is rw; method m { $.s ~= "z" } }]);
is $cat.infix.infix.operator, '~', 'the same for `~=`';

# --- write direction (EVAL) and semantics -----------------------------------

my $class = Q[class EvB { has $.n is rw; method bump { $.n += 100; $.n } }; EvB].AST.EVAL;
is $class.new(n => 1).bump, 101, 'the round-tripped method assigns through the accessor';

class B {
    has $.n is rw;
    has $.s is rw = 'a';
    method bump { $.n += 100; $.n }
    method cat  { $.s ~= 'z'; $.s }
}
is B.new(n => 1).bump, 101, 'an `is rw` attribute is updated';
is B.new.cat, 'az', 'string concatenation assigns too';

class RO {
    has $.x = 5;
    method stmt { $.x *= 2; $!x }
}
is RO.new.stmt, 5, 'a read-only accessor assigns into a throwaway container';
