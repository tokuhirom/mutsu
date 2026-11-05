use Test;
use lib 't/lib';
use MONKEY-SEE-NO-EVAL;

# `use X` declares the package `X` in its scope: a bareword `X` after it names
# that package, not a sigilless `\X` of an outer scope (#15806).

plan 8;

{
    my \SigillessShadowDep = 1;
    ok EVAL('use SigillessShadowDep; SigillessShadowDep').^name eq 'SigillessShadowDep',
        'an EVAL unit that uses X reads the package, not the caller\'s \X';
    is EVAL('{ use SigillessShadowDep; SigillessShadowDep.^name }'), 'SigillessShadowDep',
        'a block of the EVAL unit does too';
    is EVAL('use SigillessShadowDep; sub g { SigillessShadowDep.^name }; g()'), 'SigillessShadowDep',
        'and a routine of the unit';
}

# The self-referential form: the unit's own `use` runs before the binding exists.
my \SigillessShadowDep = EVAL('use SigillessShadowDep; SigillessShadowDep');
is SigillessShadowDep.^name, 'SigillessShadowDep', 'the term bound to the EVAL result is the package';

# A plain block does the same.
{
    my \Other = 5;
    {
        use SigillessShadowDep;
        is SigillessShadowDep.^name, 'SigillessShadowDep', 'a block that uses X reads the package';
    }
    is Other, 5, 'an unrelated sigilless term is untouched';
}

# A binding the unit makes after its `use` is the nearer one.
is EVAL('use SigillessShadowDep; my \SigillessShadowDep2 = 3; SigillessShadowDep2'), 3,
    'a term bound in the unit is its own';
# A pragma declares nothing.
{
    my \strict2 = 9;
    is EVAL('use v6; strict2'), 9, 'a pragma hides no outer term';
}
