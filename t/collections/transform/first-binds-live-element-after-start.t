use Test;

# `.first` on an Array answers the matching element's own container, also
# after the program has started a second thread. A `start` used to make every
# later `.first` run on a snapshot copy, so a binding to its result (or
# Red's CHECK-rewritten `@values.first(* > 1) = 20`, Red
# t/091-rakuast-compiler) wrote the copy. ADR-0068 §16.1.

plan 6;

await start { 1 };

my @a = 1, 2, 3;
my $x := @a.first(* > 1);
$x = 9;
is-deeply @a, [1, 9, 3], 'a binding to .first writes the element';

my @b = 1, 2, 3;
@b.first(* > 1) = 20;
is-deeply @b, [1, 20, 3], 'assigning to .first writes the element';

my @c = 1, 2, 3;
@c.first({ $_ > 1 }) = 30;
is-deeply @c, [1, 30, 3], 'a block matcher too';

sub through-sub {
    my @d = 1, 2, 3;
    my $y := @d.first(2);
    $y = 40;
    @d
}
is-deeply through-sub(), [1, 40, 3], 'a value matcher inside a routine';

my @e = 1, 2, 3;
@e.first({ $_ = 5; True });
is-deeply @e, [5, 2, 3], 'the matcher topic aliases the element';

my @shared = ^100;
my @done = (^4).map: { start { @shared.first(* > 50) } };
is (await @done).List, (51, 51, 51, 51), 'concurrent readers still find the element';
