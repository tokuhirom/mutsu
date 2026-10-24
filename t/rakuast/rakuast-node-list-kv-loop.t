use Test;

# RakuAST nodes compare equal to themselves: a `for @nodes.kv -> $i, $n`
# loop over a bound List read the unchanged `$n` as a write to its element
# ("Cannot modify an immutable value"), which stopped rakudo's own
# RakuAST::Deparse on every quoted string.

plan 3;

my @segments := (RakuAST::StrLiteral.new("a"), RakuAST::StrLiteral.new("b")).List;
my @seen;
for @segments.kv -> $i, $segment { @seen.push: "$i:" ~ $segment.value }
is @seen.join(','), '0:a,1:b', 'kv over a bound list of nodes';

my $n = RakuAST::IntLiteral.new(1);
ok $n eqv $n, 'a node is eqv itself';
ok RakuAST::IntLiteral.new(1) eqv RakuAST::IntLiteral.new(1), 'equal nodes are eqv';
