use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 12;

sub declaration($source) { $source.AST.statements.tail.expression }

my $angle = declaration(Q|class Angle7564:ver<1.2.3>:auth<zef:me>:api<7> { }|);
is $angle.name.colonpairs.map(*.key).join(','), 'ver,auth,api',
    'angle adverbs remain ordered colonpairs on the declaration name';
is $angle.name.colonpairs[0].value.segments[0].gist,
    'RakuAST::StrLiteral.new("1.2.3")',
    'an angle version remains a words literal';
is EVAL(Q|class Angle7564:ver<1.2.3> { }; Angle7564.^ver.Str|.AST), '1.2.3',
    'the angle version still evaluates';

constant VER7564 = '4.5.6';
my $computed = declaration(Q|class Computed7564:ver(VER7564):auth('zef:' ~ 'you'):api(3 + 4) { }|);
is $computed.name.colonpairs.map(*.key).join(','), 'ver,auth,api',
    'computed adverbs retain source order';
isa-ok $computed.name.colonpairs[0].value, RakuAST::Circumfix::Parentheses,
    'the computed name adverb retains its parentheses';
isa-ok $computed.name.colonpairs[1].value, RakuAST::Circumfix::Parentheses,
    'the computed infix adverb retains its parentheses';
isa-ok $computed.name.colonpairs[2].value, RakuAST::Circumfix::Parentheses,
    'the computed arithmetic adverb retains its parentheses';
is EVAL(Q|class Computed7564:ver(VER7564):auth('zef:' ~ 'you'):api(3 + 4) { }; Computed7564.^auth|.AST),
    'zef:you', 'computed adverb expressions evaluate after round-trip';
is EVAL(Q|class Computed7564b:api(3 + 4) { }; Computed7564b.^api|.AST), 7,
    'the computed numeric adverb evaluates';

my $role = declaration(Q|role ComputedRole7564:ver('2.0') { }|);
isa-ok $role.name.colonpairs[0].value, RakuAST::Circumfix::Parentheses,
    'a role computed adverb retains its parenthesized expression';
is EVAL(Q|role ComputedRole7564b:ver('2.0') { }; ComputedRole7564b.^ver.Str|.AST),
    '2.0', 'a computed role adverb evaluates';
is EVAL(Q|class ComputedNil7564:ver(Nil) { }; ComputedNil7564.^ver.parts.elems|.AST), 0,
    'a computed Nil adverb remains defined and part-less';
