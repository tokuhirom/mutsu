use Test;

# A plain `$var` in a token / rule body is read when the cursor reaches it
# (#14510, ADR-12917 direction 2), in the rule's declaring scope: a code block
# earlier in the body can change what it matches. The body used to be rendered
# with the variable's value spliced in before the match started. A rule's own
# parameters keep their per-call binding. Expectations were measured against
# rakudo 2026.09.

plan 9;

{
    my $x = "a";
    grammar G { token TOP { x { $x = "b" } $x } }
    ok G.parse("xb"), 'a code block\'s write is seen by a later $var';
    is $x, 'b', 'and the write reached the variable';
}

{
    sub mk() { my $m = "q"; my grammar K { token TOP { x { $m = "r" } $m } }; K }
    ok mk().parse("xr"), 'in a grammar declared in a sub';
}

{
    my $y = "p";
    grammar H { token TOP { <t>+ }; token t { $y } }
    ok H.parse("pp"), 'a subrule reads the current value';
    $y = "q";
    ok H.parse("qq"), 'and a later value on the next parse';
    nok H.parse("pp"), 'not the earlier one';
}

{
    grammar P { token TOP { <t("ab")> }; token t($v) { $v } }
    ok P.parse("ab"), 'a rule parameter is the call\'s argument';
}

{
    my $sep = ",";
    grammar L { token TOP { <w>+ % $sep }; token w { \w+ } }
    is L.parse("a,b,c")<w>.elems, 3, 'a separator variable in a rule';
    ok "a,b,c" ~~ /^ [\w+]+ % $sep $/, 'and in a regex: `% $sep` is the variable, not the `$` anchor';
}
