use Test;

plan 8;

sub attribute($source) {
    $source.AST.statements.tail.expression.body.body.statement-list.statements.head.expression
}

sub method($source) {
    $source.AST.statements.tail.expression.body.body.statement-list.statements.head.expression
}

my $attribute-trait = attribute(Q[our @co; class HandlesAttribute { has Str $.p handles @co }]).traits.head;
isa-ok $attribute-trait, RakuAST::Trait::Handles, 'an attribute keeps Trait::Handles';
isa-ok $attribute-trait.term, RakuAST::Var::Lexical, 'the attribute trait keeps its variable term';
is $attribute-trait.term.gist, 'RakuAST::Var::Lexical.new("\@co")', 'the written array variable is retained';

my $method-trait = method(Q[our @names; class HandlesMethod { method inner handles @names { "value" } }]).traits.head;
isa-ok $method-trait, RakuAST::Trait::Handles, 'a method keeps Trait::Handles';
isa-ok $method-trait.term, RakuAST::Var::Lexical, 'the method trait keeps its variable term';
is $method-trait.term.gist, 'RakuAST::Var::Lexical.new("\@names")', 'the method array variable is retained';

is EVAL(Q[our @co; BEGIN { @co = <uc lc> }; class A { has Str $.p handles @co }; A.new(p => "aB").uc].AST),
    'AB', 'EVAL delegates attribute methods named by the variable';
is EVAL(Q[our @names; BEGIN { @names = <uc lc> }; class B { method inner handles @names { "aB" } }; B.new.uc].AST),
    'AB', 'EVAL delegates method results using the variable term';
