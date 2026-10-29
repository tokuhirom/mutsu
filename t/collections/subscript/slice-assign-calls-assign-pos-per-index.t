use Test;

plan 5;

# A slice assignment to a user Positional that declares ASSIGN-POS calls it
# once per index, each with the next value, instead of passing the index list
# to a single call.
class P does Positional {
    has @!a = 10, 20, 30;
    method AT-POS(Int:D $p) is raw { @!a[$p] }
    method ASSIGN-POS(Int:D $p, \v) is raw { @!a[$p] = v }
}
my @k := P.new;

is-deeply (@k[0, 1] = 7, 8), (7, 8), 'a literal slice assigns pairwise';
is-deeply @k[0, 1].List, (7, 8), 'both slots were written';
my @i = 1, 2;
is-deeply (@k[@i] = 5, 6), (5, 6), 'a slice held in an array assigns pairwise';
is-deeply @k[@i].List, (5, 6), 'those slots were written';
@k[0] = 1;
is @k[0], 1, 'a single index still goes through ASSIGN-POS';
