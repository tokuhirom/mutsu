use v6;
use Test;

# `.classify`/`.categorize`'s mapper (and its `:as` mapper) must topicalize
# `$_` before calling a bare WhateverCode callable (`*.key`), exactly as
# `.map`/`.grep` already do per element. A bare WhateverCode closure compiles
# to a one-param Sub whose param is literally named "_", and
# `legacy_has_plain_positional_param` deliberately excludes "_" from ordinary
# positional binding for a Pair/ValuePair-shaped argument -- that shape's
# implicit argument is meant to arrive through the dynamically-scoped topic,
# not a real positional bind. Calling the mapper via `call_sub_value` alone
# left `$_` unset, so `*.key` over a Pair element saw an `Any` invocant.
# Found via Acme::Insult::Lala's
# `.flat.classify(*.key, as => *.value)`.

plan 3;

{
    my @pairs = (a => 1, b => 2);
    my %h = @pairs.classify(*.key);
    is %h.keys.sort.join(','), 'a,b', 'classify(*.key) over Pairs uses the topic';
}

{
    my @pairs = (a => "x", b => "y", a => "z");
    my %h = @pairs.classify(*.key, as => *.value);
    is %h<a>.sort.join(','), 'x,z', 'classify(*.key, as => *.value) groups by key';
}

{
    # The Acme::Insult::Lala TWEAK shape: a hyper-split/destructuring-map
    # pipeline flattened into pairs, then classified with WhateverCode.
    my %h = ("a b c", "d e f")>>.split(/\s+/)
                .map(-> [$x, $y, $z] { a => $x, b => $y, c => $z })
                .flat
                .classify(*.key, as => *.value);
    is %h.keys.sort.join(','), 'a,b,c', 'classify over a flattened hyper/map/flat pipeline';
}
