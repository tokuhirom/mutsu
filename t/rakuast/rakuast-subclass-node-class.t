use Test;
use experimental :rakuast;

# RakuAST node classes are real classes: a user class may inherit from one
# (FINALIZER's `my class LeavePhaser is RakuAST::StatementPrefix::Phaser::Leave`),
# and the phaser / block nodes it builds on are constructible.

plan 10;

isa-ok RakuAST::Block.new, RakuAST::Block, 'RakuAST::Block.new needs no body';
is RakuAST::Block.new.body.^name, 'RakuAST::Blockoid', '... it defaults to an empty blockoid';

my $leave = RakuAST::StatementPrefix::Phaser::Leave.new(RakuAST::Block.new);
isa-ok $leave, RakuAST::StatementPrefix::Phaser::Leave, 'a LEAVE phaser node wraps a block';
isa-ok RakuAST::StatementPrefix::Phaser::Enter.new(RakuAST::Block.new),
    RakuAST::StatementPrefix::Phaser::Enter, '... and so does every other phaser kind';

my class LeavePhaser is RakuAST::StatementPrefix::Phaser::Leave {
    has &!code;
    method new(&code) {
        my $phaser := callwith(RakuAST::Block.new);
        $phaser!set-code(&code);
        $phaser
    }
    method !set-code(&code --> Nil) { &!code := &code }
    method meta-object() { &!code }
}

ok LeavePhaser ~~ RakuAST::StatementPrefix::Phaser::Leave, 'a subclass of a node class is one';
ok LeavePhaser ~~ RakuAST::Node, '... and is a RakuAST::Node';
is LeavePhaser.^mro[0, 1].map(*.^name), <LeavePhaser RakuAST::StatementPrefix::Phaser::Leave>,
    'its MRO starts with itself and the node class';

my $phaser = LeavePhaser.new({ 'ran' });
is $phaser.^name, 'LeavePhaser', 'the constructor chain yields the subclass';
isa-ok $phaser, RakuAST::StatementPrefix::Phaser::Leave, '... an instance of the node class';
is $phaser.meta-object.(), 'ran', 'its own methods and attributes work';
