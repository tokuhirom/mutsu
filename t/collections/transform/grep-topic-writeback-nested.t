use Test;

# A grep callback's `$_` aliases the source element, so `$_ += ...` writes it
# back. The loop sets that alias's source per element (#19323: without
# rebuilding it when it is unchanged), so a body that runs another grep --
# which sets and restores its own -- must still write back to the outer
# element.

plan 5;

my @a = 1, 2, 3;
my @b = 5, 6;
my @r = @a.grep({ my $n = @b.grep({ $_ > 5 }).elems; $_ += $n * 10; True });
is-deeply @a, [11, 12, 13], 'the outer elements are written back after an inner grep';
is-deeply @r, [11, 12, 13], 'and the grep returns the updated elements';
is-deeply @b, [5, 6], 'the inner source is untouched';

my @c = 1..6;
is-deeply @c.grep(-> $x { $x %% 2 }).List, (2, 4, 6), 'a pointy-block grep';
is-deeply (1..10).grep(-> $p, $q { $p + $q > 10 }).List, ((5, 6), (7, 8), (9, 10)),
    'an arity-2 grep returns its chunks';
