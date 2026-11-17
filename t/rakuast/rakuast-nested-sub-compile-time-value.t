use experimental :rakuast;
use Test;

# `RakuAST::Sub.compile-time-value` of a sub declared inside a nested block,
# asked at CHECK time, is the routine: its `static_id` equals the one of the
# `&name` the block binds when it runs, as on rakudo. It was `Any`, and Red's
# `validate-code($actual, $expected)` died binding it (Red
# t/091-rakuast-compiler, "Plain helper calls retain their existing SQL
# results").

plan 7;

my ($direct, $via-lexical);
CHECK {
    sub walk($n) {
        $direct = $n.compile-time-value
            if $n ~~ RakuAST::Sub && $n.name.canonicalize eq 'increment';
        # A call's resolution is the declaration the resolver holds for it.
        $via-lexical = $n.resolution.compile-time-value
            if $n ~~ RakuAST::Call::Name && $n.name.canonicalize eq 'increment'
                && $n.is-resolved;
        $n.visit-children(&walk);
    }
    walk($*CU);
}

sub run(&block) { block() }
run {
    sub increment($value) { $value + 1 }
    isa-ok $direct, Sub, 'the declaration answers a Sub';
    is $direct.name, 'increment', 'named as declared';
    is $direct.static_id, &increment.static_id, "its static_id is the bound routine's";
    is $direct(41), 42, 'it runs the declared body';
    isa-ok $via-lexical, Sub, "a call's resolution answers a Sub too";
    is $via-lexical.static_id, &increment.static_id, 'with the same static_id';
    is increment(1), 2, 'the declared routine itself is unaffected';
};
