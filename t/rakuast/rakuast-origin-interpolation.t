use Test;

# The segments of an interpolating string carry the origins Rakudo gives
# them (ADR-10723 Stage 2): the literal text between interpolations, a
# variable where its name is written, and a `{ }` block with its blockoid,
# statement list and statements. Ranges measured on rakudo 2026.09.

plan 15;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub parts($node) { my @c; $node.visit-children(-> $c { @c.push: $c }); @c }

my $src = q:to/END/;
my $x;
say "a{ $x }b";
say "v $x w\n";
say qq{v $x};
say "{1}{2}";
say "p \$x $x";
END
my @s = $src.AST.statements;
sub segments($i) { parts(parts(parts(@s[$i].expression)[1])[0]) }

my @block = segments(1);
is range(@block[0]), '12..13', 'the text before a block';
is range(@block[1]), '13..19', 'a block';
my $blockoid = parts(@block[1])[0];
is range($blockoid), '13..19', 'its blockoid';
my $list = parts($blockoid)[0];
is range($list), '15..17', 'its statement list';
is range(parts($list)[0]), '15..17', 'the statement in it';
is range(@block[2]), '19..20', 'the text after a block';

my @var = segments(2);
is range(@var[0]), '28..30', 'the text before a variable';
is range(@var[1]), '30..32', 'a variable';
is range(@var[2]), '32..36', 'the text after it, an escape included';

is range(segments(3)[1]), '48..50', 'a variable in a qq string';

my @blocks = segments(4);
is range(@blocks[1]), '61..64', 'a block right after another';

my @escaped = segments(5);
is range(@escaped[0]), '72..78', 'text with an escaped sigil';
is range(@escaped[1]), '78..80', 'the variable after it';

my @lines;
sub walk($node) {
    @lines.push: .source.original-line(.from) with $node.origin;
    $node.visit-children(&walk);
}
q:to/END/.AST.visit-children(&walk);
my $y = 1;
say "first
second $y
third";
END
is @lines.sort.squish.join(','), '1,2,3', 'an interpolation on a later line of a string';

is EVAL(q[my $z = 2; "a{ $z + 1 }b $z"].AST), 'a3b 2', 'the round trip still runs';
