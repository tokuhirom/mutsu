use Test;

# `my ($a, $b) = RHS` predeclares its plain `$` targets so that the RHS sees
# the new variables; the statement form skips that predeclaration when the RHS
# cannot name a target (`src/compiler/destructure_lane.rs`). Every shape on
# both sides of that line keeps its answer.

plan 16;

sub two() { 'div', 5 }
{
    my ($name, $after) = two();
    is $name, 'div', 'first target from a call';
    is $after, 5, 'second target from a call';
}

{
    my @got;
    for ^3 -> $i {
        my ($x, $y) = $i, $i * 2;
        @got.push: "$x/$y";
    }
    is @got.join(' '), '0/0 1/2 2/4', 'a fresh pair of targets on every iteration';
}

{
    my $b = 'outer';
    {
        my ($a, $b) = $b, 2;
        nok $a.defined, 'the RHS sees the new, still undefined target';
        is $b, 2, '... and the target is then assigned';
    }
    is $b, 'outer', 'the outer variable is untouched';
}

{
    my $outer = 'seen';
    my ($f, $g) = { $outer }, 3;
    is $f(), 'seen', 'a closure in the RHS that names an outer variable';
}

{
    my ($first, @rest) = 1, 2, 3;
    is $first, 1, 'a scalar before a slurpy array';
    is @rest.join(','), '2,3', '... and the array slurps the rest';
}

{
    my ($p, $q) = <a b c>;
    is "$p$q", 'ab', 'surplus elements are dropped';
    my ($r, $s, $t) = 1, 2;
    nok $t.defined, 'a missing element leaves the target undefined';
}

{
    my $tally = 0;
    sub bump-it() { $tally++; 42 }
    my ($v, $w) = bump-it(), bump-it();
    is $tally, 2, 'the RHS runs once';
    is $v + $w, 84, '... and both targets get its values';
}

{
    my @log;
    my ($k, $v) = EVAL q{ 'key', 'val' };
    is "$k=$v", 'key=val', 'an EVAL on the RHS';
}

{
    my %h = a => 1;
    my ($one, $two) = %h<a>, %h<b> // 'none';
    is "$one $two", '1 none', 'subscripts on the RHS';
}

{
    my $x = 10;
    sub writes-x() { $x = 20 }
    my ($y, $z) = 1, 2;
    writes-x();
    is $x, 20, 'a sub writing an outer variable declared before the destructure';
}
