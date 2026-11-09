use v6;
use Test;

# A comparator block ending in a variable (`... ; $r`) hands back that
# variable's container (a Block returns the container of its last statement,
# #17094); the sort must read the value in it, not treat the cell as "equal"
# (Template::Jinja2's `sort(attribute=...)` filter).
plan 8;

is-deeply (3, 1, 2).sort(-> $a, $b { my $r = $a cmp $b; $r }), (1, 2, 3).Seq,
    'a pointy comparator ending in a scalar variable sorts';
is-deeply (3, 1, 2).sort(-> $a, $b { my $r = $b <=> $a; $r }), (3, 2, 1).Seq,
    'descending, numeric';
is-deeply (3, 1, 2).sort({ my $x = $^a <=> $^b; $x }), (1, 2, 3).Seq,
    'a placeholder block comparator';

my @items = ({ value => 3 }, { value => 1 }, { value => 2 });
my @attrs = 'value'.split(',');
my @sorted = @items.sort(-> $a, $b {
    my $result = 0;
    for @attrs -> $attr {
        $result = ($a{$attr} // '') cmp ($b{$attr} // '');
        last if $result != 0;
    }
    $result;
});
is @sorted.map(*<value>).join(','), '1,2,3', 'the attribute comparator of the Jinja2 sort filter';

my @a = 3, 1, 2;
@a .= sort(-> $a, $b { my $r = $a <=> $b; $r });
is-deeply @a, [1, 2, 3], '.= sort with such a comparator';

my $f = -> $a, $b { my $r = $a cmp $b; $r };
is-deeply (3, 1, 2).sort($f), (1, 2, 3).Seq, 'a comparator held in a variable';
is-deeply (3, 1, 2).sort(sub ($a, $b) { my $r = $a cmp $b; $r }), (1, 2, 3).Seq,
    'an anonymous sub comparator (unchanged)';
is-deeply (3, 1, 2).sort(-> $a, $b { my $r = $a cmp $b; $r + 0 }), (1, 2, 3).Seq,
    'a computed tail (unchanged)';
