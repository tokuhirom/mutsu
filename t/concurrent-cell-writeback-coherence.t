# Slice F (concurrent / lazy-coroutine cell) coherence: a `whenever` callback and
# a `gather` coroutine body both run synchronously on the same VM and mutate a
# captured-outer caller lexical (`my $i; whenever ... { $i++ }`,
# `my $c; gather { loop { $c++; take ... } }`) straight into `env` by name. When
# that lexical is a compiled local slot, the value must be written through to the
# slot so it stays coherent WITHOUT the reverse `sync_locals_from_env` pull. These
# pin ON == OFF == raku; run with MUTSU_NO_REVERSE_SYNC=1 to exercise the
# write-through path directly.
use Test;

plan 8;

# --- react/whenever captured-outer accumulation ----------------------------
{
    my $i = 0;
    react whenever Supply.from-list(0, 1, 2, 3, 4, 5) {
        done() if $_ == 3;
        $i++;
    }
    is $i, 3, 'blockless react: $i accumulates to the done() boundary';
}

{
    my $i = 0;
    react {
        whenever Supply.from-list(10, 20, 30) {
            $i++;
            done();
        }
    }
    is $i, 1, 'block react: $i sees the single iteration before bare done()';
}

{
    my $sum = 0;
    react whenever Supply.from-list(1, 2, 3, 4) {
        $sum += $_;
        done() if $_ == 4;
    }
    is $sum, 10, 'react: += accumulates the captured outer across all emits';
}

# --- supply on-demand / sync-emit captured-outer ---------------------------
{
    my $seen = 0;
    my $s = supply {
        for 1..3 { emit $_ }
    }
    react whenever $s { $seen++ }
    is $seen, 3, 'supply block: whenever counter accumulates over the emits';
}

# --- gather coroutine captured-outer side effect via .first ----------------
{
    my $c = 0;
    my $g = gather { my $i = 0; loop { $c++; take $i++ } };
    is $g.first(* >= 3), 3, 'gather .first returns the first matching value';
    is $c, 4, 'gather .first runs the body exactly to the match (captured $c)';
}

{
    my $c = 0;
    my $g = gather { my $i = 0; loop { $c++; take $i++ } };
    is $g.first(* > 7), 8, 'gather .first(* > 7) returns 8';
    is $c, 9, 'gather .first(* > 7) accumulates the captured $c to 9';
}
