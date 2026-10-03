use v6;
use Test;

# A role mixed into a routine keeps one attribute store, like a role mixed
# into any other value (#11459, #11203):
# - `&f does R` rebinds `&f`, so a later `&f` is the mixed-in routine;
# - a closure a role method made reads the live role attribute even after
#   that method returned;
# - a private role attribute is no accessor.

plan 10;

my role Counter {
    has $!count;
    method bump() { $!count = ($!count // 0) + 1 }
    method count() { $!count // 0 }
}

{
    sub f() { }
    &f does Counter;
    &f.bump; &f.bump;
    is &f.count, 2, '&f does R: attribute writes persist across &f mentions';
    ok &f.does(Counter), '&f still does the role';
    my $c = &f;
    $c.bump;
    is &f.count, 3, 'a copied &f shares the same attribute store';
}

{
    my role Tagged[$t] { has $.tag = $t }
    sub g() { }
    &g does Tagged['T'];
    is &g.tag, 'T', 'a public role attribute reads through &g';
}

{
    my role Lazy {
        has int $!n;
        method !set() { $!n = 7 }
        method mk() { -> { self!set; $!n } }
    }
    sub h() { }
    &h does Lazy;
    my $closure = &h.mk;
    is $closure(), 7, 'a closure made by a role method on a routine sees a later write';
    my $v = 5 but Lazy;
    my $vc = $v.mk;
    is $vc(), 7, 'the same on a value the role was mixed into';
}

{
    my role HasName { has str $!name; method inner() { self.name } }
    sub k() { }
    &k does HasName;
    is &k.name, 'k', 'a private role attribute does not shadow Routine.name';
    is &k.inner, 'k', 'self.name inside the role is the routine name too';
    my role Hidden { has $!foo = 3 }
    my $x = 5 but Hidden;
    dies-ok { $x.foo }, 'a private role attribute is no accessor on a value';
    my role Shown { has $.foo = 4 }
    is (5 but Shown).foo, 4, 'a public one still is';
}
