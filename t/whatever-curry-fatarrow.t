use v6;
use Test;

# `=>` (fat arrow) participates in Whatever-currying for NON-bareword keys.
# `* => *`, `"k" => *`, `5 => *`, `"x" ~ * => *`, `"k" => (* + 1)` all become a
# WhateverCode that yields the Pair when called. Bareword keys (`a => *`) stay a
# named-argument Pair, `xx` operands opt out, and colonpairs (`:as(*)`) stay
# literal Pairs. Regression pin for the YAMLish battery load blocker.

plan 18;

# --- type of the constructed node ---
isa-ok (* => *),         WhateverCode, '* => * is a WhateverCode';
isa-ok ("x" ~ * => *),   WhateverCode, '"x" ~ * => * is a WhateverCode';
isa-ok ("key" => *),     WhateverCode, '"key" => * is a WhateverCode';
isa-ok (5 => *),         WhateverCode, '5 => * is a WhateverCode';
isa-ok (* => 5),         WhateverCode, '* => 5 is a WhateverCode';
isa-ok ("k" => (* + 1)), WhateverCode, '"k" => (* + 1) is a WhateverCode';

# non-currying forms stay Pairs
isa-ok (a => *),      Pair, 'bareword key a => * stays a Pair';
isa-ok (a => 5),      Pair, 'bareword key a => 5 stays a Pair';
isa-ok ("key" => 5),  Pair, 'non-whatever "key" => 5 stays a Pair';
isa-ok (* xx 3 => 1), Pair, 'xx operand opts out: * xx 3 => 1 stays a Pair';
isa-ok (:as(*)),      Pair, 'colonpair :as(*) stays a Pair';

# --- calling the WhateverCode yields the right Pair ---
{
    my $c = (* => *);
    is $c('a', 'b').raku, ('a' => 'b').raku, '(* => *)("a","b") == a => b';
}
{
    my $c = (5 => *);
    is $c(9).raku, (5 => 9).raku, '(5 => *)(9) == 5 => 9';
}
{
    my $c = ("ns:" ~ * => *);
    is $c('str', 1).raku, ('ns:str' => 1).raku, 'prefix-concat key curries';
}

# --- the shape the YAMLish flattener relies on ---
{
    my %h = (str => 1, int => 2);
    my @out = %h.kv.map("ns:" ~ * => *).sort(*.key);
    is @out.elems, 2, 'kv.map(key ~ * => *) produced two pairs';
    is @out[0].raku, ('ns:int' => 2).raku, 'first flattened pair';
    is @out[1].raku, ('ns:str' => 1).raku, 'second flattened pair';
}

# a Whatever *value* under a bareword key stays an ordinary Pair value
{
    my $p = (a => 5);
    is $p.value, 5, 'bareword pair keeps its plain value';
}
