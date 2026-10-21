use Test;
use experimental :rakuast;

# RakuAST node classes are real classes: a user class may inherit from one
# (FINALIZER's `my class LeavePhaser is RakuAST::StatementPrefix::Phaser::Leave`),
# and the phaser / block nodes it builds on are constructible.

plan 26;

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
is $phaser.blorst.^name, 'RakuAST::Block', 'an inherited accessor reads the parent node';
ok $phaser.^can('blorst'), 'method introspection sees the inherited accessor';
like $phaser.gist, /^'LeavePhaser.new('/, 'gist renders the subclass constructor';
is $phaser.raku, $phaser.gist, 'raku uses the same subclass node shape';
my @children;
$phaser.visit-children({ @children.push(.^name) });
is-deeply @children, ['RakuAST::Block'], 'the inherited visitor walks the parent node';

my class StatementListChild is RakuAST::StatementList {
    method new() { callwith() }
}
my $list = StatementListChild.new;
$list.add-statement(RakuAST::Statement::Expression.new(
    expression => RakuAST::IntLiteral.new(7),
));
is $list.^name, 'StatementListChild', 'a builder subclass keeps its own class';
is $list.statements.elems, 1, 'an inherited mutator updates its parent node';
is EVAL($list), 7, 'a subclass statement list lowers through EVAL';

my class DefaultListChild is RakuAST::StatementList { }
my $default-list = DefaultListChild.new;
is $default-list.^name, 'DefaultListChild', 'an inherited constructor keeps the subclass';
is $default-list.statements.elems, 0, 'the default constructor retains an empty parent node';
is $default-list.gist, 'DefaultListChild.new()', 'the inherited node renders as the subclass';

my class OverrideListChild is RakuAST::StatementList {
    method statements() { 'own accessor' }
    method visit-children(&visitor) { 'own visitor' }
}
is OverrideListChild.new.statements, 'own accessor', 'a subclass accessor overrides the node';
is OverrideListChild.new.visit-children(-> $child { }), 'own visitor', 'a subclass method overrides the visitor';

my class NumberNode is RakuAST::IntLiteral {
    method new() { callwith(42) }
}
my $number = NumberNode.new;
my $expression = RakuAST::Statement::Expression.new(expression => $number);
is $expression.expression.^name, 'NumberNode', 'a parent node preserves its subclass child';
my @nested-children;
$expression.visit-children({ @nested-children.push(.^name) });
is-deeply @nested-children, ['NumberNode'], 'the visitor sees a subclass child';
is EVAL(RakuAST::StatementList.new($expression)), 42,
    'a nested subclass node lowers through its native parent';
