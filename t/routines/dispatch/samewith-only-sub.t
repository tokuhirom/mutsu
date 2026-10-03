use Test;

# `samewith` in an ordinary (non-multi) sub re-calls the same routine. The
# light call paths never pushed the samewith context, and neither did the
# path a routine with a `state` variable takes, so such a body died with
# "samewith called outside of a dispatch context". Reduced from
# Timezones::ZoneInfo's `timezone-data`, which follows a timezone link with
# `return %cache{$id} := samewith $_` inside an `orwith` branch.

plan 5;

sub countdown($x) { $x > 0 ?? samewith($x - 1) !! 'done' }
is countdown(3), 'done', 'plain positional sub';

sub cd2($x) { if $x > 0 { return samewith $x - 1 }; 'done2' }
is cd2(2), 'done2', 'samewith inside an if block';

sub cd3($x) { for ^1 { return samewith($x - 1) if $x > 0 }; 'done3' }
is cd3(2), 'done3', 'samewith inside a loop block';

my %links = a => 'b';
sub resolve(Str() $id --> Str) {
    state %cache;
    .return with %cache{$id};
    if $id eq 'b' { return %cache{$id} := 'got b' }
    orwith %links{$id} { return %cache{$id} := samewith $_ }
    else { 'none' }
}
is resolve('a'), 'got b', 'samewith in a routine with a state variable';
is resolve('a'), 'got b', 'second call hits the state cache';
