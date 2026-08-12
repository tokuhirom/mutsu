use v6;
use Test;

# `whenever <source> -> \row { }` — a SIGILLESS pointy param. The whenever
# statement parser only accepted sigiled variables (and type-only blocks), so
# the whole statement failed to parse and silently fragmented into a bare
# `whenever` word plus a standalone pointy block: the subscription never
# registered and the react saw zero events. Text::CSV's Supply and Channel
# in-format loops are exactly this shape (`react { whenever $in -> \row {`).

plan 4;

{
    my @in;
    react {
        whenever Supply.from-list(1, 2, 3) -> \x {
            @in.push: x;
        }
    }
    is-deeply @in, [1, 2, 3], 'whenever Supply -> \x receives every value';
}

{
    my $ch = Channel.new;
    $ch.send($_) for "a", "b";
    $ch.close;
    my @in;
    react {
        whenever $ch -> \row {
            @in.push: row;
            LAST { done }
        }
    }
    is-deeply @in, ["a", "b"], 'whenever Channel -> \row drains a pre-filled channel';
}

{
    my $ch = Channel.new;
    start {
        $ch.send($_) for 1 .. 3;
        $ch.close;
    }
    my $sum = 0;
    react {
        whenever $ch -> \n {
            $sum += n;
            LAST { done }
        }
    }
    is $sum, 6, 'whenever Channel -> \n with a concurrent producer';
}

{
    # The sigiled form keeps working.
    my @in;
    react {
        whenever Supply.from-list(<p q>) -> $v {
            @in.push: $v;
        }
    }
    is-deeply @in, [<p q>], 'whenever -> $v still works';
}

done-testing;
