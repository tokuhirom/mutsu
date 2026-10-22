use Test;

# The conditional family is built by the parser in its RakuAST shape
# (ADR-10723 Stage 2): its blocks carry the origins the parser recorded.
# Ranges measured on rakudo 2026.09.

plan 21;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub block-ranges($block) {
    (range($block), range($block.body), range($block.body.statement-list)).join(' ')
}

# --- read direction: block, blockoid and statement-list ranges ---------------
{
    my $if = "if 1 \{ 2 } elsif 3 \{ 4 } orwith 5 -> \$x \{ 6 } else -> \$y \{ 7 }\n".AST.statements[0];
    is range($if), '0..62', 'the statement keeps its own range';
    is block-ranges($if.then), '5..10 5..10 7..8', 'then block, blockoid and statement list';
    is block-ranges($if.elsifs[0].then), '19..24 19..24 21..22', 'elsif block';
    is block-ranges($if.elsifs[1].then), '34..45 40..45 42..43',
        'a pointy orwith block starts at its arrow; its blockoid at the brace';
    is block-ranges($if.else), '51..62 57..62 59..60', 'pointy else block';
}
{
    my $unless = "unless 1 \{\n  2\n}\n".AST.statements[0];
    is block-ranges($unless.body), '9..16 9..16 13..14', 'an unless body spans lines';
}
{
    my @s = "with 1 \{ 2 } orwith 3 \{ 4 } else \{ 5 }\nwithout 6 \{ 7 }\n".AST.statements;
    is block-ranges(@s[0].then), '7..12 7..12 9..10', 'with block';
    is block-ranges(@s[0].elsifs[0].then), '22..27 22..27 24..25', 'orwith block';
    is block-ranges(@s[0].else), '33..38 33..38 35..36', 'else after orwith';
    is block-ranges(@s[1].body), '49..54 49..54 51..52', 'without block';
}
{
    my $if = "if 1 \{   } else \{}\n".AST.statements[0];
    is block-ranges($if.then), '5..10 5..10 6..6', 'an empty list sits just after the brace';
    is block-ranges($if.else), '16..18 16..18 17..17', 'an empty else';
    is $if.else.body.statement-list.statements.elems, 0,
        'an empty else renders no statement of its own';
}
{
    my $if = "if 1 \{ 1; 2; }\nif 1 \{\n  1;\n  2\n}".AST.statements;
    is range($if[0].then.body.statement-list), '7..12',
        'the list ends after its last statement and its semicolon';
    is range($if[1].then.body.statement-list), '24..30', 'a list over several lines';
}

# --- visit-children: clauses are visited through their parts -----------------
{
    my @names;
    "if 1 \{ 2 } elsif 3 \{ 4 } else \{ 5 }".AST.statements[0].visit-children(
        -> $n { @names.push: $n.^name });
    is @names.join(' '),
        'RakuAST::IntLiteral RakuAST::Block RakuAST::IntLiteral RakuAST::Block RakuAST::Block',
        'an elsif contributes its condition and block, not a clause node';
}

# --- the line of every block, as Code::Coverable collects them ---------------
{
    my @lines;
    sub walk($node) {
        @lines.push: .source.original-line(.from) with $node.origin;
        $node.visit-children(&walk);
    }
    "my \$a = 1;\nif \$a \{\n  say 1;\n}\nelse \{\n  say 2;\n}\n".AST.visit-children(&walk);
    ok 5 (elem) @lines, 'the else block reports the line it starts on';
}

# --- write direction: the round trip runs as the parsed program --------------
is EVAL("my \$r; if 0 \{ \$r = 'a' } elsif 1 -> \$v \{ \$r = \"b\$v\" } else \{ \$r = 'c' }; \$r".AST),
    'b1', 'an elsif binding its tested value';
is EVAL("my \$r; with Any \{ \$r = 'a' } orwith 42 \{ \$r = \$_ } else \{ \$r = 'c' }; \$r".AST),
    42, 'an orwith topicalizes its value';
is EVAL("my \$r; with Any \{ \$r = 'a' } orwith Int \{ \$r = 'b' } else -> \$x \{ \$r = \$x.^name }; \$r".AST),
    'Int', 'an else after orwith binds the last tested value';
is EVAL("my \$r = 0; unless 0 -> \$v \{ \$r = \"u\$v\" }; \$r".AST),
    'u0', 'a pointy unless binds the condition value';
