use v6;
use Test;

plan 6;

# A call made from a closure body records the line of that statement, not the
# line of the statement the closure literal sits in (#14087). `callframe(2)` in
# `outside` is the frame of whoever called `expect`.
sub outside() { callframe(2).line }
sub expect($v) { outside() }
sub cap(&b) { b() }

my $r1 = cap({
    my $x = 1;
    expect(1)
});
is $r1, $?LINE - 2, 'last statement of a block argument';

my $r2 = cap({
    my $x = 1;
    my $y = expect(1);
    $y
});
is $r2, $?LINE - 3, 'middle statement of a block argument';

my &pointy = -> {
    my $x = 1;
    expect(1)
};
is pointy(), $?LINE - 2, 'last statement of a pointy block';

my $line = 0;
{
    my $e = $?LINE + 2;
    my $c = cap({
        $line = expect(42);
    });
    is $line, $e, 'a closure called through a try-wrapped library sub';
}

sub cap-try(&b) { my $r; try { $r = b(); CATCH { default { } } }; $r }
my $r3 = cap-try({
    my $x = 1;
    expect(1)
});
is $r3, $?LINE - 2, 'closure called inside try';

# ENTER expressions are still hoisted out of a block body that has line markers.
my $order = '';
for ^1 { LEAVE { $order ~= ENTER { 'E' } }; $order ~= 'b' }
is $order, 'bE', 'ENTER expression inside a LEAVE block';
