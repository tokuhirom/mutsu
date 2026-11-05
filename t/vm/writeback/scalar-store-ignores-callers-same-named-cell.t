use Test;

# A plain store into a routine's own scalar local does not look at the
# caller's frame: a caller that holds a shared cell under the same name (a
# variable a closure captured and mutates) is another variable, and the store
# neither writes through its cell nor falls off the fast path because of it
# (#7817: the fast path asks the frame's own overlay, not the whole chain).
plan 8;

my $c = 10;
my &bump = { $c++ };
bump();
is $c, 11, 'the captured caller variable is a shared cell';

sub callee { my $c = 0; for ^5 { $c = $c + 1 }; $c }
is callee(), 5, 'a callee loop over its own $c';
is $c, 11, 'the caller\'s cell is untouched by the callee\'s stores';

sub nested-caller {
    my $c = 100;
    my &inc = { $c = $c + 1 };
    inc();
    callee() + $c
}
is nested-caller(), 106, 'a caller routine whose own $c is a cell, calling the callee';

sub deep($n) {
    my $c = 0;
    my &cl = { $c++ };
    cl();
    if $n > 0 { deep($n - 1) }
    for ^3 { $c = $c + 10 }
    $c
}
is deep(3), 31, 'a recursive routine whose $c is a cell in every frame';

my $total = 0;
for 1..3 -> $i {
    my $c = $i;
    $c = $c * 2;
    $total += $c;
}
is $total, 12, 'a loop-local $c beside the captured one';
is $c, 11, 'and the captured variable is still 11';

my $x = 1;
my &getx = { $x };
sub plain-x { my $x = 7; $x = $x + 1; $x }
is plain-x() + getx(), 9, 'a captured variable read next to a callee\'s own store';
