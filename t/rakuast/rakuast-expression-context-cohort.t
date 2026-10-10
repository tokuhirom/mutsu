use Test;
use experimental :rakuast;

# ADR-10723 S10: declarations and modified statements retain their meaning
# when used as values. These cases also run under Rakudo 2026.07.

sub run-tree(Str $source) { EVAL($source.AST) }

is run-tree(Q[class ContextMethod { method factory { method ($n) { $n + 1 } } }; ContextMethod.new.factory.(ContextMethod.new, 4)]),
    5, 'anonymous method returned from a method body';
is run-tree(Q[class ContextBody { method { 99 }; method value { 5 } }; ContextBody.new.value]),
    5, 'anonymous method in statement position composes';
is run-tree(Q[class ContextPrivate { method !value { 7 }; method factory { method () { self!value } } }; my $c = ContextPrivate.new; $c.factory.($c)]),
    7, 'anonymous method preserves lexical private access';

is-deeply run-tree(Q[my @a = [1 if 1; 2 if 0; 3 if 1]; @a]), [1, 3],
    'array composer with several conditional sections';
is-deeply run-tree(Q[my @a = [1, 2; 3, 4]; @a]), [(1, 2), (3, 4)],
    'array composer semicolon sections retain itemization';
is-deeply run-tree(Q[my @a = [1 if 0; |[3, 4] if 1]; @a]), [3, 4],
    'slipped conditional array section';
is-deeply run-tree(Q[my @a = [$_ * 2 for 1..3; 9 if 1]; @a]), [(2, 4, 6), 9],
    'loop and conditional array sections';

is run-tree(Q[sub named(:$value) { $value }; named(:value("a".uc given 1))]), 'A',
    'given inside a colonpair value';
is run-tree(Q[sub named(:$value) { $value }; named(:value(1, 2 if 0)).raku]), 'Empty',
    'false modifier suppresses an entire named argument list';
is run-tree(Q[sub named(:$value) { $value }; named(:value(1, 2 given 3)).join(",")]), '1,2',
    'given inside a separated named argument list';

is run-tree(Q[my $f = my sub named-routine($x) { $x * 2 }; $f(4)]), 8,
    'named sub declaration in expression position';
is run-tree(Q[my $f = my proto sub dispatcher(|) {*}; multi sub dispatcher(Int $x) { $x + 2 }; $f(4)]),
    6, 'proto declaration in expression position keeps its dispatcher';

my $composer = Q{[1, 2; 3 if 1]}.AST.statements[0].expression;
isa-ok $composer, RakuAST::Circumfix::ArrayComposer;
is $composer.semilist.statements.elems, 2, 'composer retains its semicolon sections';
isa-ok $composer.semilist.statements[0].expression, RakuAST::ApplyListInfix;
is $composer.semilist.statements[1].condition-modifier.^name,
    'RakuAST::StatementModifier::If', 'section exposes its condition modifier';
is Q{[;]}.AST.statements[0].expression.semilist.statements[0].^name,
    'RakuAST::Statement::Empty', 'empty section remains an empty statement';
is-deeply run-tree(Q{[;1;;2]}), [Any, 1, Any, 2],
    'empty sections retain their values';
is-deeply run-tree(Q{[1, 2;]}), [1, 2],
    'a single section with a trailing semicolon remains flat';

my $built = RakuAST::Circumfix::ArrayComposer.new(RakuAST::SemiList.new(
    RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(4)),
    RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(5)),
));
is-deeply EVAL($built), [4, 5], 'constructed composer lowers every section';

done-testing;
