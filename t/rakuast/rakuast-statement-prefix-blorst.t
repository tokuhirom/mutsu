use Test;
use experimental :rakuast;

# Every statement prefix wraps its block-or-statement positionally and
# exposes it as `.blorst` (#9761).

plan 6;

for RakuAST::StatementPrefix::Phaser::Enter, RakuAST::StatementPrefix::Phaser::Leave,
    RakuAST::StatementPrefix::Phaser::Begin, RakuAST::StatementPrefix::Phaser::End,
    RakuAST::StatementPrefix::Phaser::First, RakuAST::StatementPrefix::Phaser::Last -> $class {
    my $node = $class.new(RakuAST::Block.new);
    isa-ok $node.blorst, RakuAST::Block, "{$class.^name}.blorst is its block";
}
