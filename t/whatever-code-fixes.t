use Test;

plan 12;

# --- Code.ACCEPTS preserves container identity (smartmatch binds topic raw) ---
{
    my $foo = "foo";
    ok $foo ~~ (* =:= $foo), 'smartmatch against WhateverCode preserves container';
    my $bar = "foo";
    ok !($foo ~~ (* =:= $bar)), 'distinct containers are not identical';
}

# --- regex code assertion may call Test functions (runs in the real interp) ---
{
    my $x = "foo";
    ok so($x ~~ /foo <?{ True }>/), 'positive code assertion succeeds';
    ok !so($x ~~ /foo <?{ False }>/), 'positive code assertion fails on False';
    ok so($x ~~ /foo <!{ False }>/), 'negative code assertion succeeds on False';
}

# --- `$_` topic scoping in WhateverCode (not clobbered by the placeholder) ---
{
    $_ = 99;
    my $wc = *.Str ~ $_;
    is $wc(5), '599', 'WhateverCode `$_` is the caller topic, not the argument';
    is-deeply (1, 2).map(*.Str ~ $_), ('199', '299'), 'map does not topicalize `$_` for a WhateverCode';
    is-deeply (do { $_ = 42; (Int).map(*.new($_)) }), (42,).Seq,
        'no scoping issues when using topic variables';
}

# --- a plain `*.abs` WhateverCode still receives the element ---
is-deeply (map *.abs, 1, -2, 3, -4), (1, 2, 3, 4),
    'plain WhateverCode still binds the element to its placeholder';

# --- `use fatal` propagates into a WhateverCode curry produced by map ---
{
    throws-like { use fatal; "a".map: *.Int }, X::Str::Numeric,
        'use fatal makes a map WhateverCode Failure throw';
    lives-ok { "a".map: *.Int }, 'without fatal, a map of Failures is a soft list';
    is-deeply ("1", "2").map(*.Int), (1, 2), 'a successful map WhateverCode is unaffected';
}
