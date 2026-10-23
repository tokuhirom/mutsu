use Test;

# Lists and the statements inside brackets carry the origins Rakudo gives
# them (ADR-10723 Stage 2): an `ApplyListInfix` spans its operands (and a
# trailing comma) and its `Infix` covers the last operator, a `SemiList`
# covers the text inside its brackets, and each `Statement::Expression` in it
# has its expression's range. Ranges measured on rakudo 2026.09.

plan 20;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub parts($node) { my @c; $node.visit-children(-> $c { @c.push: $c }); @c }

my $src = q:to/END/;
my @a = ( 1, 2 );
my @b = 1, 2,;
say [3,4, 5];
say ();
say @a[ 0 ];
my %h; say %h{"a"};
say 1 | 2 | 3;
END
my @s = $src.AST.statements;
sub arg($i) { parts(parts(@s[$i].expression)[1])[0] }

my $parens = parts(parts(@s[0].expression)[*-1])[0];
my $semi = parts($parens)[0];
is range($semi), '10..14', 'the inside of parentheses';
my $statement = parts($semi)[0];
is range($statement), '10..14', 'the statement in it';
my $list = parts($statement)[0];
is range($list), '10..14', 'a comma list';
is range(parts($list)[0]), '11..12', 'its comma';

my $trailing = parts(parts(@s[1].expression)[*-1])[0];
is range($trailing), '26..31', 'a list with a trailing comma';
is range(parts($trailing)[0]), '30..31', 'its last comma';

my $array = arg(2);
is range(parts($array)[0]), '38..44', 'the inside of an array composer';
my $items = parts(parts(parts($array)[0])[0])[0];
is range($items), '38..44', 'the list in it';
is range(parts($items)[0]), '41..42', 'its last comma';

is range(parts(arg(3))[0]), '52..52', 'the inside of empty parentheses';

my $index = parts(arg(4))[1];
is range($index), '61..66', 'an array subscript';
my $index-list = parts($index)[0];
is range($index-list), '63..64', 'the inside of it';
is range(parts($index-list)[0]), '63..64', 'the statement in it';

my $key = parts(parts(parts(@s[6].expression)[1])[0])[1];
is range($key), '81..86', 'a hash subscript';
is range(parts($key)[0]), '82..85', 'the inside of it';

my $junction = arg(7);
is range($junction), '92..101', 'a junction';
is range(parts($junction)[0]), '98..99', 'its last operator';

my @lines;
sub walk($node) {
    @lines.push: .source.original-line(.from) with $node.origin;
    $node.visit-children(&walk);
}
q:to/END/.AST.visit-children(&walk);
my @c = (
    1,
    2 ,
);
END
is @lines.sort.squish.join(','), '1,2,3', 'every line of a list is reported';
is range(parts(parts(parts(parts(q[my @d = (1, 2)].AST.statements[0].expression)[*-1])[0])[0])[0]), '9..13',
    'a list in a declaration';

is EVAL(q[my @e = (1, 2,); my %f = a => 3; @e[1] + %f{"a"}].AST), 5, 'the round trip still runs';
