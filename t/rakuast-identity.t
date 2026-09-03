use v6;
use experimental :rakuast;
use Test;

# RakuAST nodes retain object identity across Value clones while separately
# constructed copies compare structurally with eqv. This is also the identity
# contract used by the compiler's unchanged-value writeback guard.

plan 9;

my $node = RakuAST::IntLiteral.new(42);
my $alias = $node;
my $copy = RakuAST::IntLiteral.new(42);

ok $node === $alias, 'an aliased RakuAST node is === to itself';
ok $node eqv $alias, 'an aliased RakuAST node is eqv to itself';
ok $node.WHICH eqv $alias.WHICH, 'an aliased RakuAST node has stable WHICH';
nok $node === $copy, 'separately constructed RakuAST nodes are not ===';
ok $node eqv $copy, 'separately constructed equal RakuAST nodes are eqv';
nok $node.WHICH eqv $copy.WHICH, 'separately constructed nodes have distinct WHICH';

my $target = RakuAST::ParameterTarget::Var.new(name => '$x');
my $parameter = RakuAST::Parameter.new(target => $target);
my $parameter-alias = $parameter;
my $parameter-copy = RakuAST::Parameter.new(
    target => RakuAST::ParameterTarget::Var.new(name => '$x'));

ok $parameter === $parameter-alias, 'nested node aliases retain identity';
ok $parameter eqv $parameter-copy, 'nested node trees compare structurally';
nok $parameter.WHICH eqv $parameter-copy.WHICH,
    'nested separately constructed nodes have distinct WHICH';
