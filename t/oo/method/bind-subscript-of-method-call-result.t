use Test;

plan 6;

# `type.WHO<k> := v` in expression position (Red's MetamodelX::Red::Model).
class Foo {}

my \s = (Foo.WHO<R> := 5);
is s, 5, 'the bind expression yields the bound value';
is Foo.WHO<R>, 5, 'it bound into the stash';

sub bind-in(Mu \type) {
    my \symbol = (type.WHO<ResultSeq> := Proxy.new(
        FETCH => -> $ { 42 },
        STORE => -> $, $value { die "read-only" }));
    symbol
}
bind-in(Foo);
is Foo.WHO<ResultSeq>, 42, 'a Proxy bound through a sigilless invocant FETCHes';
is Foo.WHO<ResultSeq>.WHAT.^name, 'Int', 'and reads as its fetched value';

my $count = 0;
$count += (Foo.WHO<N> := 7);
is $count, 7, 'the bind is usable inside a larger expression';
is Foo.WHO<N>, 7, 'and bound';
