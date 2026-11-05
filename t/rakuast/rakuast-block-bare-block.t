use Test;

# #14916 (accessor survey): `RakuAST::Block.bare-block`, as on rakudo 2026.09.
# It is parse-time state: `True` for a block that is a whole statement
# (`{ 2 };`, nested ones too) and `False` for every other block -- a block
# used as a value, a statement's body, a phaser's block, a hand-built block
# even inside a `Statement::Expression`. `Block.new(:bare-block)` is rejected.

plan 22;

sub blocks($code) { $code.AST.find-nodes(RakuAST::Block) }
sub flags($code) { blocks($code).map(*.bare-block).list }

is-deeply flags('{ 2 };'), (True,), 'a statement-level block';
is-deeply flags('{ 2 }'), (True,), 'also without the semicolon';
is-deeply flags('{ $^a };'), (True,), 'a statement-level block with a placeholder';
is-deeply flags('{ { 3 } }'), (True, True), 'a nested statement-level block';
is-deeply flags('sub f { { 2 } }'), (True,), 'the block inside a routine body is a statement';

is-deeply flags('my $f = { 2 };'), (False,), 'a block assigned';
is-deeply flags('say { 2 }'), (False,), 'a block passed as an argument';
is-deeply flags('({ 2 })'), (False,), 'a parenthesised block';
is-deeply flags('{ 2 }.()'), (False,), 'a block with a postfix';
is-deeply flags('do { 2 };'), (False,), 'the block of do';
is-deeply flags('my $x = do { 2 };'), (False,), 'the block of an assigned do';
is-deeply flags('start { 2 }'), (False,), 'the block of start';
is-deeply flags('try { 2 }'), (False,), 'the block of try';
is-deeply flags('if 1 { 2 }'), (False,), 'the body of if';
is-deeply flags('for 1 { 2 }'), (False,), 'the body of for';
is-deeply flags('loop { last }'), (False,), 'the body of loop';
is-deeply flags('given 1 { 2 }'), (False,), 'the body of given';
is-deeply flags('BEGIN { 2 }'), (False,), 'the block of a phaser';

my $hand = RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new));
is-deeply $hand.bare-block, False, 'a hand-built block is not a bare block';
RakuAST::Statement::Expression.new(expression => $hand);
is-deeply $hand.bare-block, False, 'not even inside a statement';

# the marker is hidden state: not in the constructor form, and the statement
# still round-trips.
my $stmt = '{ 2 };'.AST.statements[0].expression;
ok !$stmt.raku.contains('bare-block'), 'the constructor form does not show it';
is EVAL('{ 2 };'.AST), 2, 'and the block still runs';
