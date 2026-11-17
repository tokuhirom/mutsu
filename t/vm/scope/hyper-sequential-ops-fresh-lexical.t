use Test;

# A hyper/race op's batch threads used to publish the lexicals they read into
# the cross-thread store, and a rollback retracted them after the join so a
# later op's fresh binding of the same name was not shadowed (#9939). The
# store holds no lexical any more (ADR-16595, #17837); these pin the behaviour
# the rollback protected.

plan 4;

my @results;
for 1..3 -> $round {
    my @search-for = $round, $round * 10;
    @results.push: (^4).hyper(:batch(1)).map({ @search-for[0] + $_ }).sum;
}
is-deeply @results, [10, 14, 18], 'each round reads its own fresh @search-for';

sub lookup(@needles) {
    my %index = @needles.map({ $_ => True });
    (^6).race(:batch(2)).grep({ %index{$_} }).sort.List
}
is-deeply lookup(<1 3>), (1, 3), 'first call reads its own %index';
is-deeply lookup(<2 4 5>), (2, 4, 5), 'a later call reads its fresh %index';

my $total = 0;
my $l = Lock.new;
for ^3 -> $r {
    my $step = $r + 1;
    (^5).race(:batch(1)).map({ $l.protect: { $total += $step } }).eager;
}
is $total, 30, 'writes made under a lock in every round land';
