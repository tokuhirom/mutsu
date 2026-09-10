use v6;
use Test;

# A `but`/`does` role-mixed value must keep its composition alive through every
# generic path a value can travel: string coercion (prefix/infix `~`,
# interpolation, `join`, `sprintf`, string comparison), `.gist`, `:=` binding,
# iteration methods, type-object naming, and composability of the built-in
# parametric roles.
#
# Anonymous roles render with a generated id (`<anon|N>`) that is not stable
# across implementations, so those are asserted by shape.

plan 56;

role Shouty { method Str { 'SHOUT' } }

# ---------------------------------------------------------------- naming

class Plain { has $.x = 1 }

is (Plain.new but Shouty).^name, 'Plain+{Shouty}', 'instance mixin name';
is (1 but Shouty).^name,         'Int+{Shouty}',   'Int mixin name';
is ('s' but Shouty).^name,       'Str+{Shouty}',   'Str mixin name';
is (%(a => 1) but Shouty).^name, 'Hash+{Shouty}',  'Hash mixin name';

{
    my @a = 3, 2, 1;
    my $r = @a but Shouty;
    is $r.^name,      'Array+{Shouty}', 'Array mixin name';
    is $r.WHAT.^name, 'Array+{Shouty}', 'Array mixin .WHAT name';
}

# An ANONYMOUS role still names the composition (it used to be masked out
# entirely, leaving a bare `Array`).
{
    my @a = 3, 2, 1;
    my $r = @a but role { method Str { self.join('<') } };
    like $r.^name,      /^ 'Array+{<anon|' \d+ '>}' $/, 'anon role mixin name';
    like $r.WHAT.^name, /^ 'Array+{<anon|' \d+ '>}' $/, 'anon role mixin .WHAT name';
}

# A PARAMETERISED role keeps its type arguments in the name.
{
    role Tagged[::T] { method tag-type { T } }
    is (5 but Tagged[Int]).^name, 'Int+{Tagged[Int]}', 'parametric role mixin name';
    is (5 but Tagged[Str]).^name, 'Int+{Tagged[Str]}', 'parametric role type arg is part of the name';
}

# ------------------------------------------------- string coercion paths

{
    my @a = 3, 2, 1;
    my $r = @a but Shouty;
    is ~$r,                'SHOUT',  'prefix ~ dispatches the mixin Str';
    is $r ~ '!',           'SHOUT!', 'infix ~ dispatches the mixin Str';
    is "$r",               'SHOUT',  'interpolation dispatches the mixin Str';
    is join('>', $r),      'SHOUT',  'join dispatches the mixin Str';
    is sprintf('%s', $r),  'SHOUT',  'sprintf %s dispatches the mixin Str';
    is $r.Str,             'SHOUT',  '.Str dispatches the mixin Str';
    ok $r eq 'SHOUT',                'eq dispatches the mixin Str';
    is [$r].map(~*).join('|'), 'SHOUT', 'coercing an array element dispatches it';
}

# The same, for every base type -- the divergence was base-type-specific.
{
    is ~(1 but Shouty),           'SHOUT', 'prefix ~ on an Int mixin';
    is join('>', 1 but Shouty),   'SHOUT', 'join on an Int mixin';
    my %h = a => 1;
    my $hm = %h but Shouty;
    is ~$hm,         'SHOUT', 'prefix ~ on a Hash mixin';
    is join('>', $hm), 'SHOUT', 'join on a Hash mixin';
    my $im = Plain.new but Shouty;
    is ~$im,         'SHOUT', 'prefix ~ on an instance mixin';
    is join('>', $im), 'SHOUT', 'join on an instance mixin';
}

# A composition WITHOUT a stringifier must keep the native rendering.
{
    role Plainish { }
    my @a = 3, 2, 1;
    my $r = @a but Plainish;
    is ~$r,           '3 2 1', 'a stringifier-less mixin keeps the native Str';
    is join('>', $r), '3 2 1', 'a stringifier-less mixin keeps the native join';
    is join('-', 1, 2, 3), '1-2-3', 'plain join is unaffected';
}

# ----------------------------------------------------------------- gist

{
    role Gisty { }
    class Foo { has $.x = 1 }
    is (Foo.new but Gisty).gist, 'Foo+{Gisty}.new(x => 1)', 'gist names the composition';
    is (Foo.new but Gisty).raku, 'Foo+{Gisty}.new(x => 1)', 'raku names the composition';
    is (Foo.new.^mixin(Gisty)).gist, 'Foo+{Gisty}.new(x => 1)', '.^mixin gists the composition';
}

# --------------------------------------------------------- `:=` binding

{
    role Bindy { }
    my @bound := (1, 2, 3) but Bindy;
    is @bound.elems, 3,  'a role-mixed list binds to an @-sigil variable';
    is @bound[1],    2,  'the bound mixin indexes through to the inner list';
    ok @bound ~~ Positional, 'the bound mixin is Positional';
}

# ------------------------------------------------- iteration / dispatch

{
    role Lastable { method last { self.sort.reverse[0] } }
    my %hp := %( 3 => 33, 4 => 44 ) but Lastable;
    is %hp.sort.elems,  2,        '.sort on a role-mixed Hash sorts its pairs';
    is %hp.sort[0].key, 3,        '.sort on a role-mixed Hash is ordered';
    is %hp.last.key,    4,        'a role method calling self.sort works';
    is %hp.keys.sort.join(','), '3,4', '.keys still works on a role-mixed Hash';
    is %hp.elems,       2,        '.elems still works on a role-mixed Hash';
}

{
    role Itery { }
    my @a = 3, 2, 1;
    my $r = @a but Itery;
    is $r.sort.join(','),          '1,2,3', '.sort on a role-mixed Array';
    is $r.map({ $_ * 2 }).join(','), '6,4,2', '.map on a role-mixed Array';
    is $r.grep({ $_ > 1 }).join(','), '3,2', '.grep on a role-mixed Array';
}

# ------------------------------------------------------- sink context

{
    my $sunk = 0;
    role Sinky { method sink { $sunk++ } }
    1 but Sinky;
    is $sunk, 1, 'a composed .sink runs when a `but` statement is sunk';
    (2) does Sinky;
    is $sunk, 2, 'a composed .sink runs for a `does` statement too';

    my $marked = 0;
    role Quiet { }
    1 but Quiet;
    is $marked, 0, 'a composition without .sink sinks silently';
}

# ----------------------------------------- built-in (parametric) roles

{
    my %h := %( 2 => 3 ) but Associative[Int, Int];
    is %h.^name, 'Hash+{Associative[Int,Int]}', 'a built-in parametric role composes';
    is (5 but Numeric).^name,  'Int+{Numeric}',  'a bare built-in role composes';
    is (5 but Callable).^name, 'Int+{Callable}', 'Callable composes too';
}

# ------------------------------------ value mixins vs. real allomorphs

# `1 but "hi"` composes an ANONYMOUS role in raku; it is NOT the IntStr
# allomorph, and it does not do Str.
{
    my $forty-two = 42 but 'forty two';
    is $forty-two + 33, 75, 'a value mixin keeps its numeric value';
    like $forty-two.^name, /^ 'Int+{<anon|' \d+ '>}' $/, 'a value mixin names an anonymous role';
    is $forty-two.Str, 'forty two', 'a value mixin overrides .Str';
    nok $forty-two ~~ Str, 'a value mixin does NOT do the mixed value type';

    nok (1 but True) ~~ Bool, 'a Bool value mixin does not do Bool either';
    is (5 but False).clone.Bool, False, 'a value mixin survives .clone';

    # A genuine allomorph is untouched by all of the above.
    is <42>.^name, 'IntStr', 'a real allomorph keeps its allomorph type';
    ok <42> ~~ Str, 'a real allomorph does Str';
    is <42>.Str, '42', 'a real allomorph stringifies to its literal';
}
