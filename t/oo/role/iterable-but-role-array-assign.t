use Test;

plan 6;

# `Q.new but role {...}` keeps the wrapped Iterable class's own `iterator`
# for `@`-assignment and `for` (#15973).
class Q does Iterable { method iterator { (4, 5).iterator } }

my @b = Q.new but role { method of { 1 } };
is @b.elems, 2, '@-assign of an Iterable instance mixed with a role iterates it';
is-deeply @b.List, (4, 5), 'the elements come from the class iterator';

my @seen;
for Q.new but role { } { @seen.push: $_ }
is-deeply @seen, [4, 5], 'for over the mixed Iterable instance iterates it';

my @a = Q.new;
is @a.elems, 2, 'the bare instance still decomposes';

my $x = Q.new but role { };
my @c = $x;
is @c.elems, 1, 'a scalar-held mixed instance stays one item';

my $y = (Q.new but role { method iterator { (7, 8, 9).iterator } });
my @d = $y;
is @d.elems, 1, 'scalar-held with its own role iterator stays one item';
