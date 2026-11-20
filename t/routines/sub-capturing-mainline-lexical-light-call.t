use Test;

# A named sub that captures a mainline lexical (ADR-0024) is served by the
# light / positional-light / fast call paths, which push a routine frame of
# their own (ADR-0037 Slice 1). Its captured reads and writes still resolve
# through the mainline cell, not through whatever the caller's frame holds.

my $count = 0;
my $cache = Map.new;

sub bump() { $count++ }
sub get-count() { $count }
sub seen(Mu \key) { $cache{key} }
sub remember(Mu \key, Mu \value) {
    my %next = $cache;
    %next{key} = value;
    $cache = %next.Map;
    value
}
sub lookup(Mu \M, Mu \T --> Mu) {
    my \key = M.WHICH ~ '|' ~ T.WHICH;
    my \hit = $cache{key};
    hit.defined ?? hit !! remember(key, M.^name ~ "/" ~ T.^name);
}
sub add-to($n) { $count += $n; $count }

bump() for ^5;
is $count, 5, 'a zero-argument sub writes the mainline lexical';
is get-count(), 5, 'and reads it back';

is lookup(Int, Str), 'Int/Str', 'a sigilless-parameter sub publishes into the captured cache';
is lookup(Int, Str), 'Int/Str', 'and finds the entry on the next call';
is seen(Int.WHICH ~ '|' ~ Str.WHICH), 'Int/Str', 'another sub sees the published entry';
is $cache.elems, 1, 'the mainline sees the published entry';

is add-to(10), 15, 'a positional-argument sub updates the captured lexical';
is $count, 15, 'the mainline sees the update';

# A block that shadows the captured name: the sub keeps its own capture, and
# the caller's shadow is untouched (ADR-0024 row 2a).
{
    my $count = 100;
    bump();
    is $count, 100, 'a shadowing block keeps its own variable';
    is get-count(), 16, 'the sub reads the mainline variable, not the shadow';
    is add-to(1), 17, 'and writes the mainline variable';
    is $count, 100, 'the shadow is still untouched';
}
is $count, 17, 'the mainline variable has every write';

# Called from inside another routine and from a closure.
sub outer-caller() { bump(); get-count() }
is outer-caller(), 18, 'a call from another routine';
my &later = -> { add-to(2) };
is later(), 20, 'a call from a closure';

my @seen = (^4).map: { lookup(Int, $_ %% 2 ?? Str !! Num) };
is @seen.join(','), 'Int/Str,Int/Num,Int/Str,Int/Num', 'calls from a map block';
is $cache.elems, 2, 'two distinct keys were published';

done-testing;
