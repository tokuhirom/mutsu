use Test;

# Prefix and postfix operators, ternaries, contextualizers and pairs carry
# the origins Rakudo gives them (ADR-10723 Stage 2): the operator as
# written, a ternary across its three operands, the statements inside a
# contextualizer's parentheses, and a pair from its key or colon. Ranges
# measured on rakudo 2026.09.

plan 16;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub parts($node) { my @c; $node.visit-children(-> $c { @c.push: $c }); @c }

my $src = q:to/END/;
my $x = 1;
say -$x;
say $x++;
say $x ?? 1 !! 2;
say %(a => 1, b => 2);
say $(1);
my %h = :b(2);
END
my @s = $src.AST.statements;
sub arg($i) { parts(parts(@s[$i].expression)[1])[0] }

is range(parts(arg(1))[0]), '15..16', 'a prefix operator';
is range(parts(arg(2))[1]), '26..28', 'a postfix operator';

my $ternary = arg(3);
is range($ternary), '34..46', 'a ternary';
is range(parts($ternary)[2]), '45..46', 'its else branch';

my $hash = arg(4);
is range($hash), '52..69', 'a hash contextualizer';
my $sequence = parts($hash)[0];
is range($sequence), '54..68', 'the inside of its parentheses';
my $statement = parts($sequence)[0];
is range($statement), '54..68', 'the statement in it';
my $pairs = parts($statement)[0];
is range($pairs), '54..68', 'the pairs in it';
is range(parts($pairs)[1]), '54..60', 'a pair';
is range(parts($pairs)[2]), '62..68', 'another pair';

my $item = arg(5);
is range(parts($item)[0]), '77..78', 'the inside of an item contextualizer';

my $colonpair = parts(parts(@s[6].expression)[*-1])[0];
is range($colonpair), '89..94', 'a valued colonpair';
my $value = parts($colonpair)[0];
is range($value), '91..94', 'its parenthesized value';
is range(parts($value)[0]), '92..93', 'the inside of it';

my @lines;
sub walk($node) {
    @lines.push: .source.original-line(.from) with $node.origin;
    $node.visit-children(&walk);
}
q:to/END/.AST.visit-children(&walk);
my $t = True
    ?? 1
    !! 2;
END
is @lines.sort.squish.join(','), '1,2,3', 'a ternary over several lines';

is EVAL(q[my $q = 3; my %p = %(a => -$q); $q++ ?? %p<a> !! 0].AST), -3, 'the round trip still runs';
