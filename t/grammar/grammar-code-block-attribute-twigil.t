use Test;

# A grammar rule's code block or code assertion that reads an attribute
# (`$!n`, `$.n`) inside an expression gets the rule's cursor as `self`, as one
# that names `self` already did (#10730). It used to die with "Variable $!n
# used where no 'self' is available". The start rule's blocks see the built
# invocant `.parse` makes (#10848), so its defaults apply; a subrule's cursor
# is minted without BUILD, so its attribute is uninitialised, as in rakudo.

plan 8;

# The ticket's repro.
grammar R { has $.n = 1; token TOP { <?{ $!n }> a { @*seen.push: $!n } } }
{
    my @*seen;
    is ~R.parse("a"), 'a', '<?{ $!n }> in the start rule sees the default';
    is-deeply @*seen, [1], '{ $!n } in the start rule reads it too';
}

grammar G {
    has $.n = 1;
    has @.list;
    token TOP { <t> }
    token t { a { @*seen.push: ($!n // 'undef'); @*seen.push: $.n.raku } }
}

{
    my @*seen;
    lives-ok { G.parse("a") }, '$!n and $.n in a code block do not die';
    is-deeply @*seen, ['undef', 'Any'], 'a subrule cursor\'s attribute is uninitialised';
}

grammar A {
    has $.n;
    token TOP { <t> }
    token t { <?{ $!n.defined }> a | b }
}
is ~A.parse("b"), 'b', '<?{ $!n... }> runs against the cursor';

grammar N {
    has $.n;
    token TOP { <t> }
    token t { <!{ $!n }> a }
}
is ~N.parse("a"), 'a', '<!{ $!n }> runs against the cursor';

grammar E {
    token TOP { a { @*seen.push: ($! // 'none') } }
}
{
    my @*seen;
    E.parse("a");
    is-deeply @*seen, ['none'], '$! alone is still the error variable';
}

grammar L {
    has @.items;
    token TOP { <t> }
    token t { a { @*seen.push: @!items.elems } }
}
{
    my @*seen;
    L.parse("a");
    is-deeply @*seen, [0], 'an @! attribute reads the cursor too';
}
