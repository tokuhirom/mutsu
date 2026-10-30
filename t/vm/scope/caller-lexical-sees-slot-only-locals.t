use Test;

# `throws-like` evaluates a string in its caller's frame (`CALLER::` as the
# EVAL context), so a main program local that nothing else names must reach it
# with its latest value although the stores to it write no env entry (#15048):
# the call pushes the calling frame's slot-only locals into the entry the reader
# sees. This file holds no EVAL or CALLER spelling itself.

plan 7;

my $x = 1;
throws-like 'die $x', X::AdHoc, payload => '1', 'the declared value';
$x = 2;
throws-like 'die $x', X::AdHoc, payload => '2', 'the latest store';
$x++;
throws-like 'die $x', X::AdHoc, payload => '3', 'after a ++';
$x ~= 'a';
throws-like 'die $x', X::AdHoc, payload => '3a', 'after a ~=';

my @list = 1, 2, 3;
@list.push(4);
throws-like 'die @list.elems', X::AdHoc, payload => '4', 'a slot-only array';

for 1..2 -> $i {
    my $z = $i * 10;
    throws-like 'die $z', X::AdHoc, payload => ($i * 10).Str, "a loop-body local, iteration $i";
}
