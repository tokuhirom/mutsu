use Test;

# An attribute's `handles` clause in RakuAST, measured on rakudo 2026.09: a
# `Trait::Handles` holding the written term. mutsu's parser keeps that term
# beside the delegation specs it reads from it; EVAL derives the specs from
# the term again.

plan 9;

sub attr($src) {
    $src.AST.statements.head.expression.body.body.statement-list.statements.head.expression
}

my $named = attr(Q[class A1 { has $.x handles 'baz' }]);
isa-ok $named.traits.head, RakuAST::Trait::Handles, '`handles` is a Trait::Handles';
isa-ok $named.traits.head.term, RakuAST::QuotedString, 'holding the written name';
isa-ok attr(Q[class A2 { has $.x handles * }]).traits.head.term, RakuAST::Term::Whatever,
    '`handles *` holds a Whatever';

is EVAL(Q[class A { has $.x handles <uc lc> }; A.new(x => "Ab").uc].AST),
    'AB', 'a word list survives the round trip';
is EVAL(Q[class B { has @.l handles "elems" }; B.new(l => (1, 2, 3)).elems].AST),
    3, 'so does one quoted name';
is EVAL(Q[class C { has $.s handles * }; C.new(s => "hey").chars].AST),
    3, 'and a wildcard';
is EVAL(Q[class D { has $.x handles ('uc', 'lc') }; D.new(x => 'q').lc ~ D.new(x => 'q').uc].AST),
    'qQ', 'and a parenthesised list';
throws-like { EVAL(Q[class F { has $.x handles <uc>; method uc { "own" } }].AST) },
    Exception, 'a method of the same name still clashes with the delegated one';
throws-like { EVAL(Q[class G { has $.x handles <uc> }; G.new(x => "z").lc].AST) },
    X::Method::NotFound, 'a method not named is not delegated';
