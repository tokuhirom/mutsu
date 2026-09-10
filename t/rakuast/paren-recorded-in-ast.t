use Test;

# The parser records EVERY `(...)`, because parenthesization is a property of
# the source rather than of whichever downstream consumer happens to care. That
# is what lets `.AST` render rakudo's `Circumfix::Parentheses(SemiList(...))`,
# and it is why each semantic row below has to keep working with the marker in
# place. Everything here was checked against `raku` first; this file must pass
# under both `raku` and `mutsu`.

plan 36;

# --- the AST says what the source said ---------------------------------------

ok Q{my $x = (1, 2)}.AST.gist.contains('Circumfix::Parentheses'),
    'a parenthesized list keeps its parentheses in the AST';
ok Q{my $x = (1)}.AST.gist.contains('Circumfix::Parentheses'),
    'a parenthesized single term keeps its parentheses';
ok Q{say (1, 2)}.AST.gist.contains('Circumfix::Parentheses'),
    'not initializer-specific';
ok Q{(a => 1)}.AST.gist.contains('Circumfix::Parentheses'),
    'a parenthesized bareword pair keeps its parentheses';
ok Q{("a" => 1)}.AST.gist.contains('Circumfix::Parentheses'),
    'a parenthesized quoted-key pair keeps its parentheses';
ok Q{my $y = (1 + 2) * 3}.AST.gist.contains('Circumfix::Parentheses'),
    'parentheses that only group still appear';
nok Q{my $x = 1 + 2}.AST.gist.contains('Circumfix::Parentheses'),
    'an unparenthesized expression has none';

# --- parentheses stay transparent to evaluation ------------------------------

is (1 + 2) * 3, 9, 'grouping still drives precedence';
is-deeply ((1, 2), (3, 4)).elems, 2, 'nested parens still nest';
is-deeply (my @a = (1, 2, 3)).elems, 3, 'a parenthesized list assigns flat';

# `($a) = ...` is a LIST assignment: the lone target slurps the whole RHS.
{
    my $a;
    ($a) = 1, 2, 3;
    is $a.elems, 3, 'a parenthesized scalar target is a list-assignment target';
}

# --- lvalues written inside parentheses --------------------------------------

{
    my $i = 0;
    ($i)++;
    is $i, 1, 'postfix ++ through parentheses mutates the variable';
    --($i);
    is $i, 0, 'prefix -- through parentheses mutates the variable';
}

{
    # A declaration in expression position, subscripted: the parentheses group,
    # they do not build a value of their own.
    my @slots;
    (my @inner = 0 xx 4)[2] = 77;
    @slots = @inner;
    is @slots[2], 77, 'a subscript-assign through a parenthesized declaration writes';
}

{
    my $a = 'apple';
    my $b = 'blueberry';
    my @r = (($a, $b) »~=» <pie tart>);
    is @r.join(','), 'applepie,blueberrytart', 'hyper meta-assignment returns the new values';
    is $a, 'applepie', 'and writes back through the parenthesized lvalue list';
}

{
    my $a;
    my $b;
    for ($a, $b) -> \x, \y { x = 9; y = 8 }
    is-deeply $a, 9, 'a parenthesized for-source still writes back per slot';
    is-deeply $b, 8, 'both slots';
}

# --- enum bodies -------------------------------------------------------------

{
    my $e = enum (a => 5, b => 10);
    is $e.^name, 'Map', 'a parenthesized anon enum is a Map';
    is a.value, 5, 'and its explicit values survive';
}

# --- parentheses are transparent to identity and to phaser timing ------------

{
    # A parenthesized list holds each scalar's container, and a slice of it
    # keeps them: `=:=` must still find `$foo`'s own container.
    my $foo = 42;
    ok ($foo, "x", 17)[0] =:= $foo, 'a parenthesized list keeps element containers';
    ok ($foo, "x", 17)[0, 1][0] =:= $foo, 'and a slice of one keeps them too';
}

# A phaser inside a parenthesized expression keeps its normal timing: `INIT`
# runs at initialisation, outside the `gather`, so the `take` has no gather.
dies-ok { EVAL '(gather for 1..3 { INIT take "OH"; take $_ })' },
    'INIT inside a parenthesized gather still runs outside it';

# --- Whatever currying: the layer count is the freeze ------------------------
#
# `(*)` is a curry point and `((*))` is a frozen `Whatever` VALUE. Adding a
# second pair of parentheses is the only thing that distinguishes them, so this
# is where the marker's placement is load-bearing.

is (* + 1).WHAT.gist, '(WhateverCode)', '(* + 1) curries';
is ((* + 1)).WHAT.gist, '(WhateverCode)', 'an extra layer does not freeze a composed curry';
is (*).WHAT.gist, '(Whatever)', '(*) is the Whatever value';
is ((*)).WHAT.gist, '(Whatever)', '((*)) is too';
is ((*.so)).WHAT.gist, '(WhateverCode)', 'an extra layer around a curried postfix does not freeze it';

{
    my $f = (* - 1) - 1;
    is $f(6), 4, 'a parenthesized WhateverCode composes into a larger one';
}

is (* xx 2).raku, '(*, *).Seq', '`* xx 2` repeats the Whatever value';

# The freeze finishes a closure rather than growing it: `.assuming` lands on the
# WhateverCode itself.
is ((*.flip)).assuming(42)(), '24', 'an extra layer finishes the closure for .assuming';
is ((* + *)).assuming(42)(3), 45, 'and for a two-parameter one';

# A `**` follows the same rule as a `*`.
is (**).WHAT.gist, '(HyperWhatever)', '(**) is the HyperWhatever value';
is ((**)).WHAT.gist, '(HyperWhatever)', 'so is ((**))';
ok ((**)) ~~ HyperWhatever:D, 'and it is a defined value, not a type object';

# Smartmatch: a bare `*` on the right autoprimes, a parenthesized one is a value.
ok (Mu ~~ (*)), 'a parenthesized Whatever on the RHS of ~~ is a value that matches';
