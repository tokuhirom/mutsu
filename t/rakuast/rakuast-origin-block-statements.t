use Test;

# Loops, topicalizers and bare blocks are built by the parser in their RakuAST
# shape (ADR-10723 Stage 2): their blocks carry the origins the parser
# recorded. Ranges measured on rakudo 2026.09.

plan 16;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub block-ranges($block) {
    (range($block), range($block.body), range($block.body.statement-list)).join(' ')
}

my @s = q:to/END/.AST.statements;
for 1, 2 -> $x { 1 }
while 0 { 2 }
until 1 { 3 }
loop (my $i = 0; $i < 1; $i++) { 4 }
repeat { 5 } while 0;
given 1 { when 1 { 6 }; default { 7 } }
{ 8 }
for ^2 { 9 }
END

# --- read direction ------------------------------------------------------------
is block-ranges(@s[0].body), '9..20 15..20 17..18', 'a pointy for block starts at its arrow';
is block-ranges(@s[1].body), '29..34 29..34 31..32', 'while';
is block-ranges(@s[2].body), '43..48 43..48 45..46', 'until';
is block-ranges(@s[3].body), '80..85 80..85 82..83', 'a C-style loop';
is block-ranges(@s[4].body), '93..98 93..98 95..96', 'repeat ... while';
is block-ranges(@s[5].body), '116..147 116..147 118..145', 'given';
my @in-given = @s[5].body.body.statement-list.statements;
is block-ranges(@in-given[0].body), '125..130 125..130 127..128', 'when';
is block-ranges(@in-given[1].body), '140..145 140..145 142..143', 'default';
is block-ranges(@s[6].expression), '148..153 148..153 150..151', 'a bare block statement';
is block-ranges(@s[7].body), '161..166 161..166 163..164', 'a for over a plain block';

{
    my @lines = "while 0 \{\n  1\n}\nrepeat while 0 -> \$x \{\n  2\n}\n".AST.statements.map: {
        .body.origin.source.original-line(.body.origin.from)
    };
    is @lines.join(','), '1,4', 'a block reports the line it starts on';
    is range("repeat while 0 -> \$x \{ 2 }".AST.statements[0].body), '15..26',
        'a pointy repeat block starts at its arrow';
}

# --- visit-children: a repeat visits its condition first ---------------------
{
    my @names;
    "repeat \{ 1 } while 2".AST.statements[0].visit-children(-> $n { @names.push: $n.^name });
    is @names.join(' '), 'RakuAST::IntLiteral RakuAST::Block', 'condition, then body';
}

# --- write direction: the round trip runs as the parsed program --------------
is EVAL(q[my $n = 0; my $s = ''; repeat { $s ~= $n } until ++$n >= 3; $s].AST), '012',
    'repeat until';
is EVAL(q[my @r; FOO: loop (my $i = 0; $i < 5; $i++) { last FOO if $i == 3; @r.push: $i }; @r.join].AST),
    '012', 'a labelled C-style loop';
is EVAL(q[given 5 { when * > 3 { "big $_" }; default { 'small' } }].AST), 'big 5',
    'given, when and default';
