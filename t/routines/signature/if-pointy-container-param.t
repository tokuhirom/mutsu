use Test;

plan 16;

# `if EXPR -> @a { }` tests EXPR itself and only BINDS the container when the
# branch is taken. Assigning the condition into an `@`/`%` container instead
# changed its truthiness (`my @a = Any` is a one-element `[Any]`, which is
# true), so a missing hash element ran the block and handed out an `Any`.

my %cache = full => [1, 2, 3], empty => [], zero => 0;

my @seen;
for <full empty zero missing> -> $k {
    if %cache{$k} -> @avail {
        @seen.push($k ~ ':' ~ @avail.elems);
    }
    else {
        @seen.push($k ~ ':else');
    }
}
is @seen.join(' '), 'full:3 empty:else zero:else missing:else',
        'array pointy tests the condition, not the bound container';

# The pointy parameter binds, so the elements are the condition's own.
if %cache<full> -> @avail {
    is @avail.elems, 3, 'bound array has the condition array elements';
    is @avail[1], 2, 'bound array indexes into the condition array';
    ok @avail ~~ Positional, 'bound array is Positional';
}
else {
    flunk 'truthy array condition entered the else branch' for ^3;
}

# Same for a hash-sigil pointy parameter.
my %outer = inner => { p => 1, q => 2 };
if %outer<inner> -> %got {
    is %got<p>, 1, 'bound hash sees the condition hash';
    is %got.elems, 2, 'bound hash has the condition hash keys';
}
else {
    flunk 'truthy hash condition entered the else branch' for ^2;
}
nok (if %outer<absent> -> %got { True }), 'missing hash element is false for a hash pointy';

# Value (expression) position takes the same path.
my $r = do if %cache<missing> -> @a { 'then' } else { 'else' };
is $r, 'else', 'value-position if with an array pointy tests the condition';
my $r2 = do if %cache<full> -> @a { @a.elems } else { 'else' };
is $r2, 3, 'value-position if with an array pointy binds the container';

# elsif carries its own binding.
sub pick($v) {
    if $v -> @a { "then:{@a.elems}" }
    elsif %cache<full> -> @b { "elsif:{@b.elems}" }
    else { 'else' }
}
is pick([7, 8]), 'then:2', 'array pointy on the if branch';
is pick(Any), 'elsif:3', 'array pointy on the elsif branch';
is pick(0), 'elsif:3', 'falsy condition falls through to the elsif';

# A slurpy array parameter is NOT the plain-container binding above: it has to
# reach the real signature binder so the slurpy/one-argument rules apply.
# `+@a` in particular used to look "simple" to the parser and take the plain
# `my @a := COND` route, which cannot apply the single-argument rule
# (roast S04-statements/if.t "slurpy parameters on block").
if 1, 2 -> +@a { is-deeply @a, [1, 2], '+@ applies the single-argument rule' }
if 42   -> +@a { is-deeply @a, [42],  '+@ with one argument' }
if 1, 2 -> *@a { is-deeply @a, [1, 2], '*@ flattens the condition list' }
if 1, 2 -> **@a { is-deeply @a, [(1, 2),], '**@ keeps the condition list intact' }
