use Test;

# `PointyBlock.replace-signature` swaps the signature of a pointy block (not only
# of a routine); `set-implicit-topic` marks a block as taking its statement's topic.
plan 4;

my $b = RakuAST::PointyBlock.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new));
ok $b.can('replace-signature'), 'PointyBlock can replace-signature';
ok $b.can('set-implicit-topic'), 'a block can set-implicit-topic';

my $p = RakuAST::Parameter.new(target => RakuAST::ParameterTarget::Var.new(:name('$_')));
$b.replace-signature(RakuAST::Signature.new(parameters => ($p,)));
is $b.signature.parameters.elems, 1, 'the new signature has its parameter';

$b.set-implicit-topic(True, :required);
is-deeply ($b.implicit-topic, $b.required-topic), (True, True), 'the topic flags read back';
