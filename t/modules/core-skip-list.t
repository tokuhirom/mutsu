# The core list routine is shadowed by Test's skip directive after `use Test`.
# Keep its behavior in a standalone TAP test so both native and real Test
# providers exercise the same core call.

say '1..5';

my $test = 0;
sub check($got, $expected, $description) {
    $test++;
    if $got.join('|') eq $expected.join('|') {
        say "ok $test - $description";
    }
    else {
        say "not ok $test - $description";
    }
}

check skip(2, <a b c d e>), <c d e>, 'skip(Int, List) skips leading values';
check skip(0, <a b c>), <a b c>, 'skip(0, List) skips nothing';
check skip(5, <a b c>), (), 'skip(N, List) past the end is empty';

my @array = <x y z w>;
check skip(2, @array), <z w>, 'skip(Int, @array) flattens the array operand';
check <a b c d>.skip(1), <b c d>, 'List.skip(N) method form works';
