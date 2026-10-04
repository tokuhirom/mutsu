use Test;

# A closure a role method returns reads `$!a` after the method returned. On a
# value (or routine) composing several roles the attribute is seeded under each
# role's key, while the role's private method writes only its own role's key;
# the read must take the declaring role's key, every run.

plan 4;

role N {
    has int $!a;
    method mk { -> { self!s unless $!a; $!a } }
    method !s { $!a = 3 }
}
role S { method sym { 1 } }
class C { }

{
    my $o = C.new but S;
    $o = $o but N;
    is $o.mk()(), 3, 'value with two roles: closure reads the written attribute';
}

{
    my @seen = (^10).map: {
        my $o = (C.new but S) but N;
        $o.mk()();
    };
    is-deeply @seen.unique.List, (3,), 'the read is the same on every run';
}

{
    sub g { }
    &g does S;
    &g does N;
    is &g.mk()(), 3, 'routine with two roles: closure reads the written attribute';
}

{
    role P { has $!a = 'p'; method mkp { -> { $!a } } }
    my @seen = (^10).map: {
        my $o = (C.new but N) but P;
        $o.mk()() ~ ' ' ~ $o.mkp()();
    };
    is-deeply @seen.unique.List, ('3 p',),
        "two roles' same-named attributes stay apart, read from each role's closure";
}
