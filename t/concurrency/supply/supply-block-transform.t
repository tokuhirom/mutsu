use Test;

plan 6;

# A `supply { whenever $upstream { emit ... } }` block is a transform: it taps
# an upstream supply and re-emits downstream. Consuming it via react must deliver
# the transformed values. (Regression: the inner whenever's emits were dropped.)

sub doubler($in) {
    supply {
        whenever $in -> $x { emit $x * 2; }
    }
}
sub adder($in) {
    supply {
        whenever $in -> $x { emit $x + 1; }
    }
}

# Single-stage transform fed from a from-list supply.
my @single;
react {
    whenever doubler(Supply.from-list(1, 2, 3)) -> $v { @single.push($v) }
}
is @single.join(","), "2,4,6", "single supply-block transform delivers values";

# Two-stage transform chain: doubler -> adder.
my @chain;
my $s1 = doubler(Supply.from-list(1, 2, 3));
my $s2 = adder($s1);
react {
    whenever $s2 -> $v { @chain.push($v) }
}
is @chain.join(","), "3,5,7", "chained supply-block transforms deliver values";

# Three-stage chain of distinct transforms.
sub minus3($in) {
    supply {
        whenever $in -> $x { emit $x - 3; }
    }
}
my @three;
react {
    whenever minus3(doubler(adder(Supply.from-list(0, 10)))) -> $v { @three.push($v) }
}
is @three.join(","), "-1,19", "three-stage supply-block transform chain";

# Inline supply block (not from a sub) still works.
my @inline;
my $in = Supply.from-list(5, 6);
my $out = supply { whenever $in -> $x { emit $x * 10 } };
react { whenever $out -> $v { @inline.push($v) } }
is @inline.join(","), "50,60", "inline supply-block transform";

# A bare-emit supply block (no whenever) is unaffected.
my @bare;
react { whenever supply { emit 1; emit 2; } -> $v { @bare.push($v) } }
is @bare.join(","), "1,2", "bare-emit supply block still works";

# Filtering transform: only even values pass through.
sub evens($in) {
    supply {
        whenever $in -> $x { emit $x if $x %% 2; }
    }
}
my @filtered;
react {
    whenever evens(Supply.from-list(1, 2, 3, 4, 5, 6)) -> $v { @filtered.push($v) }
}
is @filtered.join(","), "2,4,6", "filtering supply-block transform";
