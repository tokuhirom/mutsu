use Test;

plan 10;

my $assignment = Q[(my $value) = 1, 2, 3].AST.statements[0].expression;
isa-ok $assignment, RakuAST::ApplyInfix,
    'a parenthesized declaration assignment remains an ApplyInfix';
isa-ok $assignment.left, RakuAST::Circumfix::Parentheses,
    'the declaration keeps its parenthesized source form';
my $assigned-count;
(my $value) = 1, 2, 3;
$assigned-count = $value.elems;
is $assigned-count, 3,
    'the declaration assignment retains list assignment semantics';

my $star = Q[my @items; (@items, *) = 1, 2, 3].AST.statements[1].expression;
isa-ok $star, RakuAST::ApplyInfix,
    'a Whatever list target remains an assignment';
isa-ok $star.left, RakuAST::Circumfix::Parentheses,
    'the list target keeps its parentheses';
ok $star.left.gist.contains('RakuAST::ApplyListInfix.new')
    && $star.left.gist.contains('RakuAST::Term::Whatever.new'),
    'the star target remains a Whatever term';

my $quoted-dotty = Q[my $text = 'a'; $text .= 'uc'()].AST.statements[1].expression;
isa-ok $quoted-dotty, RakuAST::ApplyDottyInfix,
    'quoted method names retain the dotty assignment node';
ok $quoted-dotty.right.gist.contains('RakuAST::Call::QuotedMethod'),
    'the quoted method name remains in the call node';

my $indirect = Q[my $object; my $name = 'value'; $object."$name"() = 1].AST
    .statements[2].expression;
isa-ok $indirect, RakuAST::ApplyInfix,
    'an indirect method lvalue remains a plain assignment';
ok $indirect.left.gist.contains('RakuAST::Call::QuotedMethod'),
    'the interpolated method name remains attached to the lvalue call';
