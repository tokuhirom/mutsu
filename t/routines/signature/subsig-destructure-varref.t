use Test;

plan 12;

# Positional destructuring of a list bound by reference (single-argument rule).
sub pos2(($x, $y)) { $x + $y }
{
    my @a = (3, 4);
    is pos2(@a), 7, 'positional destructure of @array variable';
}
{
    my $items = (3, 4);
    is pos2($items), 7, 'positional destructure of itemized list variable';
}
is pos2([3, 4]), 7, 'positional destructure of array literal';

# Three-element list var.
sub pos3(($a, $b, $c)) { "$a-$b-$c" }
{
    my @t = (1, 2, 3);
    is pos3(@t), '1-2-3', 'three-element positional destructure of @array';
}

# Named destructuring of a list of pairs.
sub named2((:$foo, :$bar)) { $foo + $bar }
is named2((foo => 10, bar => 20)), 30, 'named destructure of pair-list literal';
{
    my @list = (foo => 10, bar => 20);
    is named2(@list), 30, 'named destructure of @array of pairs';
}

# Named destructuring of a Hash variable.
sub named-h((:$a, :$b)) { $a * $b }
{
    my %h = a => 3, b => 4;
    is named-h(%h), 12, 'named destructure of %hash variable';
}
is named-h({ a => 3, b => 4 }), 12, 'named destructure of hash literal';

# Named destructure with a default.
sub named-def((:$x, :$y = 99)) { "$x/$y" }
{
    my @l = (x => 5,);
    is named-def(@l), '5/99', 'named destructure applies default for missing key';
}

# Subsignature on a named array parameter (single-argument rule).
sub typed-pos(@p ($x, $y)) { $x * $y }
{
    my @pt = (6, 7);
    is typed-pos(@pt), 42, 'subsignature on @-sigil param with array variable';
}

# Named destructure picks up keys regardless of order.
sub conn((:$host, :$port)) { "$host:$port" }
{
    my %config = port => 8080, host => "example";
    is conn(%config), 'example:8080', 'named destructure of hash var, key order independent';
}

# A pair-list with non-Str keys.
sub vpairs((:$one, :$two)) { ($one // 0) + ($two // 0) }
{
    my @pl = (one => 100, two => 200);
    is vpairs(@pl), 300, 'named destructure picks both keys from pair-list var';
}
