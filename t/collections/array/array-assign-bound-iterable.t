use Test;

plan 5;

class Sequence does Iterable {
    method iterator { (1, 2, 3).iterator }
}

my $bound := Sequence.new;
my @from-bound = $bound;
is-deeply @from-bound, [1, 2, 3],
    'a bound scalar has no item container and iterates its Iterable value';

my @from-direct = Sequence.new;
is-deeply @from-direct, [1, 2, 3],
    'direct assignment uses the same Iterable iterator';

my $item = Sequence.new;
my @from-item = $item;
is @from-item.elems, 1, 'an ordinarily assigned scalar stays one item';
ok @from-item[0] === $item, 'the item retains its instance identity';

my @reassigned;
@reassigned = $bound;
is-deeply @reassigned, [1, 2, 3],
    'assignment to an existing array also iterates a bound scalar';
