use v6;
use Test;

# A for-loop's pointy param must not stay bound when the loop exits
# abnormally (`return` from inside the body, an exception, an outer loop's
# `last`). The normal path restores the prior binding via a post-phaser
# opcode that an unwinding frame skips; the leftover final-iteration value
# then leaked out of the routine into a same-named caller binding via the
# method env merge. Text::CSV: RangeSet.in's `for @!ranges -> $r { ... and
# return True }` clobbered method CSV's `while $in() -> $r` param, turning
# every CSV row into the RangeSet's internal Pair.

plan 3;

class RS {
    has Pair @!ranges;
    method add(Pair $p) { @!ranges.push: $p }
    method in(Int $i) {
        for @!ranges -> $r {
            $i >= $r.key && $i <= $r.value and return True;
        }
        False;
    }
}

{
    my $rs = RS.new;
    $rs.add(1 => 100);
    my @seen;
    my @rows = ["a", "b"], ["c", "d"], ["e", "f"];
    my $i = 0;
    my $n = 0;
    while @rows[$n++] -> $r {
        if $rs.in($i++) {
            @seen.push: $r;
        }
        last if $n >= 3;
    }
    is-deeply @seen, [["c", "d"], ["e", "f"]],
        'caller loop param survives a callee method returning from inside its own for';
}

{
    sub inner($x) {
        for 10, 20, 30 -> $r {
            return "inner:$r" if $r == 20;
        }
        "none";
    }
    my @got;
    for 1, 2 -> $r {
        my $res = inner($r);
        @got.push: "$r/$res";
    }
    is-deeply @got, ["1/inner:20", "2/inner:20"],
        'sub returning from inside a for does not clobber the caller loop param';
}

{
    my $outer-r;
    sub thrower {
        for 1, 2, 3 -> $r {
            die "boom" if $r == 2;
        }
    }
    for 42 -> $r {
        try thrower();
        $outer-r = $r;
    }
    is $outer-r, 42, 'exception unwind restores the loop param binding';
}

done-testing;
