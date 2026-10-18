use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 8;

sub declaration($source) { $source.AST.statements.tail.expression }

my $class = declaration(Q|class QualifiedAdverb7564::Package:ver<1.0> { }|);
is $class.name.parts.map(*.name).join('::'), 'QualifiedAdverb7564::Package',
    'qualified declaration name keeps its parts';
is $class.name.colonpairs.map(*.key).join(','), 'ver',
    'qualified declaration name carries its adverb';
isa-ok $class.name.colonpairs[0], RakuAST::ColonPair::Value,
    'the name adverb is a value colonpair';
is EVAL(Q|class QualifiedAdverb7564::Eval:ver<1.0> { }; QualifiedAdverb7564::Eval.^ver.Str|.AST),
    '1.0', 'a qualified class adverb evaluates after round-trip';

my $role = declaration(Q|role QualifiedAdverb7564::Role:ver('2.0') { }|);
is $role.name.parts.map(*.name).join('::'), 'QualifiedAdverb7564::Role',
    'qualified role name keeps its parts';
is $role.name.colonpairs.map(*.key).join(','), 'ver',
    'qualified role name carries its adverb';
isa-ok $role.name.colonpairs[0].value, RakuAST::Circumfix::Parentheses,
    'the computed role adverb keeps its parentheses';
is EVAL(Q|role QualifiedAdverb7564::EvalRole:ver('2.0') { }; QualifiedAdverb7564::EvalRole.^ver.Str|.AST),
    '2.0', 'a qualified role adverb evaluates after round-trip';
