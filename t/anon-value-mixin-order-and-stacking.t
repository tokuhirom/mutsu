use Test;

plan 14;

role A { }

# `but <non-role>` composes a fresh ANONYMOUS role, and each application is its
# own composition -- so two of them stack, and they take their place in
# application order among the named roles rather than always coming last.
# The `<anon|N>` counter is per-process, so the assertions match the SHAPE.

like ((1 but "x") but A).^name, /^ 'Int+{<anon|' \d+ '>}+{A}' $/,
    'an anonymous role applied first is named first';
like ((1 but A) but "x").^name, /^ 'Int+{A}+{<anon|' \d+ '>}' $/,
    'an anonymous role applied last is named last';
like ((1 but "x") but "y").^name, /^ 'Int+{<anon|' \d+ '>}+{<anon|' \d+ '>}' $/,
    'two anonymous roles stack instead of collapsing';
like (1 but "x").^name, /^ 'Int+{<anon|' \d+ '>}' $/,
    'a single anonymous role is unchanged';

# The two anonymous roles are DISTINCT, not the same name twice.
{
    my $name = ((1 but "x") but "y").^name;
    my @anon = $name.comb(/'<anon|' \d+ '>'/);
    is @anon.elems, 2, 'the stacked name carries two anonymous roles';
    isnt @anon[0], @anon[1], 'and they are two different roles';
}

# `.^roles` lists them in the same (last-first) order the name does.
{
    my @roles = ((1 but "x") but "y").^roles.map(*.^name);
    is @roles.elems, 4, '.^roles lists both anonymous roles plus Real and Numeric';
    like @roles[0], /^ '<anon|' \d+ '>' $/, 'the last-applied anonymous role comes first';
    like @roles[1], /^ '<anon|' \d+ '>' $/, 'the first-applied one comes next';
    isnt @roles[0], @roles[1], 'and they are distinct';
}

# A genuine allomorph is still an allomorph: the value-mixin marker's other
# job -- telling the two apart -- is unaffected.
is <42>.^name, 'IntStr', 'a literal allomorph still reports IntStr';
is (<42> but A).^name, 'IntStr+{A}', 'an allomorph with a role keeps its allomorph name';
ok (<42> ~~ Str), 'an allomorph still does Str';
nok ((1 but "x") ~~ Str), 'a value mixin does not';
