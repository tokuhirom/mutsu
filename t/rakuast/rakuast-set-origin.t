use Test;

# RakuAST::Node.set-origin($origin): the node's origin becomes the given
# RakuAST::Origin (the very object) and is answered back; it takes only an
# origin.

plan 9;

my $ast = Q[my $x = 1;
say $x;].AST;
my $origin = $ast.statements[1].origin;
isa-ok $origin, RakuAST::Origin, 'a parsed statement has an origin';

my $node = RakuAST::IntLiteral.new(5);
nok $node.origin.defined, 'a hand-built node has no origin';

my $answer = $node.set-origin($origin);
isa-ok $answer, RakuAST::Origin, 'set-origin answers an origin';
ok $answer === $origin, 'it answers the origin it was given';
ok $node.origin === $origin, 'the node now has that very origin';
is $node.origin.from, $origin.from, 'its start';
is $node.origin.to, $origin.to, 'its end';

my $other = RakuAST::StrLiteral.new("a");
$other.set-origin($origin);
is $other.origin.from, $origin.from, 'any node takes an origin';

dies-ok { $node.set-origin(42) }, 'only an origin is accepted';

done-testing;
