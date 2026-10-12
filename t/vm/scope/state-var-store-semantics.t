use Test;

# The `state` store is keyed by (variable, closure-clone scope). Pin that
# the per-key behaviour holds: persistence across calls, independence of
# distinct variables, and a fresh store per closure clone.

plan 6;

sub counter { state $n = 0; ++$n }
counter() for ^4;
is counter(), 5, 'named-sub state persists across calls';

sub two { state $a = 10; state $b = 20; $a++; $b += 2; "$a/$b" }
two();
is two(), '12/24', 'distinct state vars in one sub stay independent';

sub anon-state { $ = 42 }
my $last;
$last = anon-state() for ^1000;
is $last, 42, 'anonymous state assignment in a hot loop';

my @subs = (^3).map: -> $i { sub { state $c = $i * 100; ++$c } };
@subs[0]() for ^3;
is @subs[0](), 4, 'first closure clone has its own counter';
is @subs[1](), 101, 'second closure clone starts from its own initializer';
is @subs[2](), 201, 'third closure clone is unaffected by the others';
