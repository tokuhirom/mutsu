use Test;

# A LAST phaser runs after the final iteration and still sees that
# iteration's loop parameters, for any number of `for` parameters.
plan 6;

my @seen;
for (10, 20, 30).kv -> $p, $v {
    next if $p == 1;
    LAST { @seen.push("p=$p v=$v") }
}
is @seen.join(","), "p=2 v=30", "two scalar parameters, ending on the last element";

@seen = ();
for <a b c>.kv -> $i, $e {
    next if $i == 0;
    LAST { @seen.push("i=$i e=$e") }
}
is @seen.join(","), "i=2 e=c", "kv pairs with next";

@seen = ();
for 1..6 -> $a, $b, $c {
    LAST { @seen.push("$a-$b-$c") }
}
is @seen.join(","), "4-5-6", "three parameters";

@seen = ();
for 1..3 -> $n { LAST { @seen.push("n=$n") } }
is @seen.join(","), "n=3", "single parameter";

@seen = ();
for 1..5 -> $x, $y {
    last if $x >= 3;
    LAST { @seen.push("x=$x") }
}
is @seen.join(","), "x=3", "left through `last`";

@seen = ();
for () -> $x, $y { LAST { @seen.push("ran") } }
is @seen.elems, 0, "an empty loop does not run LAST";
