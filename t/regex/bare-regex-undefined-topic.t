use Test;

# A bare `/.../` (or `rx/.../`) in statement or boolean position, with an
# undefined `$_`, never starts a match: it is False, `$/` stays Nil and the
# regex's code blocks do not run. `m/.../` and `$_ ~~ /.../` are explicit
# matches and still run (with the uninitialized warning).

plan 12;

my @ran;
{
    /{ @ran.push: 'bare' } x/;
    rx/{ @ran.push: 'rx' } x/;
    rx :s { { @ran.push: 'rx-s' } x };
    is @ran.elems, 0, 'bare regex statements do not run their code blocks';
    is $/.raku, 'Nil', '$/ stays Nil';
}

{
    is (so /x*/), False, 'so /x*/ is False although x* matches the empty string';
    is ?(/x*/), False, 'the boolean context too';
    is (/x*/ ?? 't' !! 'f'), 'f', 'a ternary condition';
    is (not /x*/), True, 'and negated';
    if /^$/ { flunk 'if /^$/ matched an undefined topic' } else { pass 'if /^$/ does not match an undefined topic' }
}

{
    my @r;
    my $out = quietly so m/{ @r.push: 'm' } x*/;
    is $out, True, 'm/x*/ is an explicit match against the empty string';
    is @r.elems, 1, '... and runs its code block';
}

{
    $_ = 'x';
    my @d;
    /{ @d.push: 'def' } x/;
    is @d.elems, 1, 'a defined topic matches';
    is $/.Str, 'x', '... and sets $/';
}

{
    $_ = '';
    is (so /x*/), True, 'an empty-string topic is defined and matches';
}
