use Test;

plan 6;

# #13049: a closure that calls a block-declared sub binds the declaring
# activation's code object. The binding is read from the routine the
# activation just registered; these pin that it stays per activation.

my @keep;
for 1..3 -> $i {
    my $q = $i * 10;
    sub h($x) { $q + $x }
    @keep.push: -> $y { h($y) };
}
is-deeply @keep.map({ $_(1) }).List, (11, 21, 31), 'each closure sees its own activation of a block sub';

my @maps;
for 1..3 -> $i {
    my $q = $i * 100;
    sub h2($x) { $q + $x }
    @maps.push: (1, 2).map({ h2($_) }).List;
}
is-deeply @maps.List, ((101, 102), (201, 202), (301, 302)), 'an inline map callback binds the activation';

{
    package Inner {
        my @got;
        for 1..2 -> $i {
            my $q = $i;
            sub k($x) { $q * $x }
            @got.push: (-> $y { k($y) });
        }
        our sub run() { @got.map({ $_(10) }).List }
    }
    is-deeply Inner::run(), (10, 20), 'a block sub inside a package body binds its activation';
}

my @cells;
for 1..3 -> $i {
    my $w = 0;
    sub bump() { $w++; $w }
    @cells.push: -> { bump() };
}
is-deeply @cells.map({ $_() }).List, (1, 1, 1), 'a block sub writing a free variable keeps one counter per activation';
is-deeply @cells.map({ $_() }).List, (2, 2, 2), 'and the counters persist across calls';

my @late;
for 1..2 -> $i {
    my $q = $i;
    sub twice($x) { $q * 2 + $x }
    my &f = &twice;
    @late.push: f(0);
}
is-deeply @late.List, (2, 4), 'a code object taken by name in the declaring block is that activation\'s';
