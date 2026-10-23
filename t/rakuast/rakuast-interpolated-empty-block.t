use Test;

# An empty `{ }` inside an interpolating string is a real (empty) `Block`
# in rakudo's tree, not dropped text (#14820).

plan 8;

my $string = 'say "a {  } b"'.AST.statements[0].expression.args.args[0];
is $string.^name, 'RakuAST::QuotedString', 'string with an empty block is a QuotedString';
my @segments = $string.segments;
is @segments.elems, 3, 'text, block, text';
is @segments[1].^name, 'RakuAST::Block', 'the middle segment is a Block';
is @segments[1].body.^name, 'RakuAST::Blockoid', 'the block has a Blockoid body';
is @segments[1].body.statement-list.statements.elems, 0, 'with no statements';

my $only = 'say "{}"'.AST.statements[0].expression.args.args[0];
is $only.segments[0].^name, 'RakuAST::Block', '"{}" is a single Block segment';

{
    my $warned = False;
    CONTROL { when CX::Warn { $warned = True; .resume } }
    is "a {  } b", 'a  b', 'an empty block interpolates as nothing';
    ok $warned, 'and warns about Nil in string context, like rakudo';
}
