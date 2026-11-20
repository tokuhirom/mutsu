use Test;

# A routine whose body has a `for` loop is called on the same fast and light
# call paths as any other routine (a `for` no longer counts as an inner sub).
# Each shape below is called repeatedly, so later calls run on the cached path.

plan 14;

sub zero-arg() { my @a = 1, 2, 3; my $s = 0; for @a { $s += $_ }; $s }
is (zero-arg() for ^3).join(','), '6,6,6', 'zero-argument sub with a for loop';

sub positional(@p) { my $s = ''; for @p { $s ~= $_ }; $s }
is (positional(<a b>) for ^3).join(','), 'ab,ab,ab', 'positional sub with a for loop';

sub named(:@items) { my $n = 0; for @items { $n++ }; $n }
is (named(:items(1, 2)) for ^3).join(','), '2,2,2', 'named sub with a for loop';

sub early(@p) { for @p { return $_ if $_ > 1 }; 'none' }
is (early((1, 2, 3)) for ^2).join(','), '2,2', 'return from inside the loop leaves the sub';
is early((0, 1)), 'none', 'falls through when nothing returns';

sub phasers(@p) {
    my @log;
    for @p {
        FIRST @log.push('first');
        NEXT @log.push("next$_");
        LAST @log.push('last');
    }
    @log.join(' ')
}
is phasers((1, 2)), 'first next1 next2 last', 'loop phasers run';
is phasers((1, 2)), 'first next1 next2 last', 'and run again on a second call';

sub counter(@p) { state $calls = 0; $calls++; for @p { } ; $calls }
counter(()) for ^2;
is counter((1,)), 3, 'state survives across calls of a sub with a loop';

sub depth($n) { return 0 if $n == 0; my $m = 0; for ^1 { $m = depth($n - 1) + 1 }; $m }
is depth(5), 5, 'recursion through a loop body';

sub writes(@p) { for @p { $_ *= 2 }; @p }
my @w = 1, 2;
writes(@w);
is @w.join(','), '2,4', 'the topic writes back into the caller array';

sub last-next(@p) { my @out; for @p { next if $_ == 2; last if $_ == 4; @out.push($_) }; @out.join(',') }
is (last-next((1, 2, 3, 4, 5)) for ^2).join('|'), '1,3|1,3', 'next and last';

sub outer-topic(@p) { my @seen; for @p { @seen.push($_) }; $_ }
$_ = 'kept';
outer-topic((1, 2));
is $_, 'kept', 'the caller topic is untouched';

class C {
    has @.items = 1, 2, 3;
    method total() { my $t = 0; for @!items { $t += $_ }; $t }
}
my $c = C.new;
is ($c.total for ^3).join(','), '6,6,6', 'method with a loop over an attribute';

sub closure-in-loop(@p) { my @subs; for @p -> $x { @subs.push({ $x * 10 }) }; @subs».().join(',') }
is closure-in-loop((1, 2)), '10,20', 'closures created in the loop keep their own binding';
