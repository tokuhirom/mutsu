use v6;
use Test;

plan 8;

# A block with an explicit signature binds the iteration element to its
# parameters, NOT to $_ — its $_ stays the enclosing topic (all expectations
# verified against rakudo).

for <a b> {
    my @hits = <a b c>.grep(-> $c { $c eq $_ });
    is-deeply @hits, [$_], "grep pointy-block \$_ is the outer topic ($_)";
}

for "o" {
    is-deeply <a b>.map(-> $c { $c ~ $_ }).List, ("ao", "bo"),
        'map pointy-block $_ is the outer topic';
    is-deeply <a b>.map({ $^x ~ $_ }).List, ("ao", "bo"),
        'map placeholder-block $_ is the outer topic';
    is-deeply <a b>.map(-> $c, $d { $c ~ $d ~ $_ }).List, ("abo",),
        'arity-2 pointy-block $_ is the outer topic';
    is-deeply <a b>.map({ $_ }).List, ("a", "b"),
        'bare block still topicalizes the element';
}

# WhateverCode: a plain *.foo takes the element; one referencing $_ keeps the
# outer topic (S02 "no scoping issues when using topic variables").
is-deeply (1, 2, 3).grep(* > 1).List, (2, 3), 'plain WhateverCode takes the element';
is-deeply (do { $_ = 42; (Int,).map(*.new($_)).List }), (42,),
    'WhateverCode referencing $_ keeps the outer topic';
