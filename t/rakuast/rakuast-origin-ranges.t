use Test;

# ADR-10723 §4: a statement's `origin` is Rakudo's `{ from, to }` character
# range over the unit's source, and `.source.original-line` finds the line of
# an offset. Every expectation below was measured on rakudo 2026.09.

plan 16;

sub ranges($ast) {
    my @r;
    sub walk($node) {
        with $node.?origin {
            @r.push: (.from => ($node.^name.subst('RakuAST::', '') ~ " {.from}..{.to} L{.source.original-line(.from)}"))
                if $node.^name.contains('Statement::');
        }
        $node.visit-children(&walk);
    }
    walk($ast);
    @r.sort(*.key).map(*.value).List
}

# The range excludes the leading whitespace, the `;` and the trailing space.
my $src = "my \$x = 1;\nif \$x \{\n  say 1;\n\} else \{\n  say 2\n\}; say 3; say 4\n";
my $ast = $src.AST;
my @s = $ast.statements;
is @s[0].origin.from, 0, 'the first statement starts at offset 0';
is @s[0].origin.to, 9, 'its range stops before the `;`';
is @s[1].origin.from, 11, 'a compound statement starts at its keyword';
is @s[1].origin.to, 46, 'and ends at its closing brace';
is @s[2].origin.from, 48, 'statements on one line have their own ranges';
is @s[3].origin.from, 55, 'the last of them too';
is @s[3].origin.source.original-line(@s[3].origin.from), 6, 'original-line maps an offset to its line';

# Nested statements keep their own ranges.
is-deeply ranges($ast).List,
    ('Statement::Expression 0..9 L1', 'Statement::If 11..46 L2', 'Statement::Expression 21..26 L3',
     'Statement::Expression 39..44 L5', 'Statement::Expression 48..53 L6', 'Statement::Expression 55..60 L6'),
    'every statement of a unit, nested ones included';

# The source is the unit's text, shared by every origin.
is @s[0].origin.source.orig, $src, '.source.orig is the text of the unit';
is @s[0].origin.source.original-line(0), 1, 'offset 0 is on line 1';
is @s[0].origin.source.original-line(12), 2, 'an offset inside line 2';
is-deeply ranges("say 1;\nsay 2;\n\n  say 3".AST),
    ('Statement::Expression 0..5 L1', 'Statement::Expression 7..12 L2', 'Statement::Expression 17..22 L4'),
    'blank lines and indentation are skipped';

# Offsets count characters, not bytes.
is-deeply ranges("say 'é日'; say 2".AST),
    ('Statement::Expression 0..8 L1', 'Statement::Expression 10..15 L1'),
    'a multi-byte character is one position';

# A combining mark joins its letter and `\r\n` is one character, as in rakudo.
is-deeply ranges("say 'ae\x[301]b';\r\nsay 'c'; say 1".AST),
    ('Statement::Expression 0..9 L1', 'Statement::Expression 11..18 L2', 'Statement::Expression 20..25 L2'),
    'graphemes are single positions';

# A hand-built node has no origin.
nok RakuAST::IntLiteral.new(1).?origin.defined, 'a hand-built node has no origin';
is-deeply ranges("use v6.d;\nsay 'é'; say 2".AST),
    ('Statement::LanguageVersion 0..8 L1', 'Statement::Expression 10..17 L2', 'Statement::Expression 19..24 L2'),
    'a pragma statement is placed too';
