use Test;

plan 6;

# A loop body's `my` starts each iteration as a fresh `Any`. Skipping that
# reset is only allowed where nothing can observe it (#9537); these are the
# observers that must still see the fresh binding.

my @out;
for 1..3 -> $i { my $y = $i == 2 ?? 7 !! die "x"; CATCH { default { @out.push($y.raku) } } }
is @out.join(' '), 'Any Any', 'CATCH in a for body sees a failed declaration as Any';

@out = ();
my $n = 0;
while $n++ < 3 { my $w = $n == 2 ?? 7 !! die "x"; CATCH { default { @out.push($w.raku) } } }
is @out.join(' '), 'Any Any', 'CATCH in a while body sees a failed declaration as Any';

@out = ();
for 1..3 -> $i { my $y = $i == 2 ?? 7 !! die "x"; CATCH { default { @out.push(EVAL('$y.raku')) } } }
is @out.join(' '), 'Any Any', 'EVAL in CATCH sees a failed declaration as Any';

@out = ();
for 1..3 { my $y = ({ $y // 0 })() + 1; @out.push($y) }
is @out.join(' '), '1 1 1', 'a closure in the initializer sees the fresh binding';

@out = ();
sub peek { $*d // 'Any' }
for 1..3 -> $i { my $*d = "{peek()}-$i"; @out.push($*d) }
is @out.join(' '), 'Any-1 Any-2 Any-3', 'a dynamic declaration is fresh for its initializer';

my $t = 0;
for ^100 -> $i { my $a = $i * 2; my $b = $a mod 7; $t += $a + $b }
is $t, 10196, 'plain arithmetic declarations keep their values';
