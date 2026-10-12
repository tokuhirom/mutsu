use Test;

plan 5;

# `(my $y := EXPR)` in a loop body declares a fresh cell per iteration, so an
# earlier `$q := $y` keeps its value when `$y` is declared again.

my $q;
my $k = 0;
my $seen;
while $k < 2 {
    (my $y := $k + 100);
    if $k == 0 { $q := $y } else { $seen = "$q/$y" }
    $k++;
}
is $seen, "100/101", "while loop, parenthesised bind";

my $r;
$k = 0;
my $seen2;
while $k < 2 {
    my $y := $k + 100;
    if $k == 0 { $r := $y } else { $seen2 = "$r/$y" }
    $k++;
}
is $seen2, "100/101", "while loop, statement bind";

# The shape Hash::int's `push` uses: remember the previous pulled item.
sub pairs-of(+values) {
    my $iterator := values.iterator;
    my $previous := Nil;
    my @out;
    until ((my $pulled := $iterator.pull-one)) =:= IterationEnd {
        if $previous =:= Nil {
            $previous := $pulled;
        }
        else {
            @out.push: ($previous, $pulled);
            $previous := Nil;
        }
    }
    @out
}
is-deeply pairs-of(1, "a", 2, "b"), [(1, "a"), (2, "b")], "previous item survives the next pull";

my @kept;
for 1..3 -> $i {
    (my $x := $i * 10);
    @kept.push: $x;
}
is-deeply @kept, [10, 20, 30], "for loop, parenthesised bind";

my $v := 1;
(my $w := $v);
$v := 2;
is $w, 1, "outside a loop";

done-testing;
