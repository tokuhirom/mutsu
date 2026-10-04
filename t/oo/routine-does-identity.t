use Test;

# ADR-11827: a routine is an object; `does` on it composes in place, so every
# alias of the routine sees the role.

plan 12;

role R { method hi { 'hi' } }
role Sym[$name] { method native_symbol { $name } }
role Keep[$routine] { method kept { $routine } }

{
    sub f { 42 }
    my $g = &f;
    &f does R;
    ok $g ~~ R, 'an alias taken before does sees the role';
    is $g.hi, 'hi', 'the alias dispatches to the role method';
    ok &f ~~ R, 'a later &f rebuild sees the role';
    is $g(), 42, 'the routine still runs';
}

{
    sub h { }
    &h does Keep[&h];
    &h does Sym['qsort'];
    is &h.kept.native_symbol, 'qsort',
        'a role argument that captured the routine sees a later does';
}

{
    my $c = -> { 7 };
    my $alias = $c;
    $c does R;
    is $alias.hi, 'hi', 'does on a closure is seen through its alias';
}

{
    sub k { }
    my $copy = &k.clone;
    &k does R;
    nok $copy ~~ R, 'a clone taken before does does not get the role';
    ok &k ~~ R, 'the original has the role';
}

{
    sub m { }
    &m does R;
    my $copy = &m.clone;
    ok $copy ~~ R, 'a clone starts from the current composition';
    role Q { method q { 'q' } }
    $copy does Q;
    nok &m ~~ Q, 'does on the clone does not reach the original';
}

{
    role Counter { has $.n = 0; method bump { $!n++ } }
    sub c { }
    my $old = &c;
    &c does Counter;
    &c.bump;
    &c does R;
    $old.bump;
    is &c.n, 2, 'successive does keep one attribute store';
    is $old.n, 2, 'the store is shared by an earlier alias';
}
