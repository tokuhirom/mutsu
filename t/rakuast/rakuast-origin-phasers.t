use Test;

# Phaser blocks carry the origins the parser read them from (ADR-10723
# Stage 2): the phaser, and its Block / Blockoid / StatementList. Ranges
# measured on rakudo 2026.09.

plan 12;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub first-child($node) { my @c; $node.visit-children(-> $c { @c.push: $c }); @c[0] }

my $src = q:to/END/;
BEGIN { 1 }
END {
    2;
}
for 1 { LAST {} }
INIT say 4;
END
my @s = $src.AST.statements;

my $begin = @s[0].expression;
is range($begin), '0..11', 'a BEGIN phaser';
is range(first-child($begin)), '6..11', 'its block';
is range(first-child($begin).body), '6..11', 'its blockoid';
is range(first-child($begin).body.statement-list), '8..9', 'its statement list';

my $end = @s[1].expression;
is range($end), '12..26', 'an END phaser over several lines';
is range(first-child($end)), '16..26', 'its block';
is range(first-child($end).body.statement-list), '22..24',
    'its statement list, the `;` included';

my $last = @s[2].body.body.statement-list.statements[0].expression;
is range($last), '35..42', 'a LAST phaser in a loop body';
is range(first-child($last)), '40..42', 'its empty block';
is range(first-child($last).body.statement-list), '41..41', 'an empty statement list';

is range(@s[3].expression), '45..55', 'a phaser over a bare statement';

is EVAL(q[my $x = 0; BEGIN { 1 }; for 1..3 { $x += $_; LAST { $x *= 2 } }; $x].AST), 12,
    'the round trip still runs';
