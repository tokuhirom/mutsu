use Test;

# Two roles mixed into one object may each declare `$!a`; the object is
# reblessed into `C+{N}+{P}` and each level keeps its own copy. A closure a
# role method makes reads and writes the copy of the level that composed its
# role, as the role's methods do (#17890, #17936).

plan 6;

class C { }
role N {
    has $!a = 3;
    method m { $!a }
    method mk { -> { $!a } }
    method set { -> $v { $!a = $v } }
}
role P { has $!a = 'p'; method pm { $!a }; method pmk { -> { $!a } } }

{
    my $o = C.new;
    $o does N;
    $o does P;
    is "{$o.m} {$o.mk()()} {$o.pm} {$o.pmk()()}", '3 3 p p',
        'in-place does: each closure reads its own role level';
    $o.set()(7);
    is "{$o.m} {$o.mk()()} {$o.pm}", '7 7 p', 'a write through the closure reaches that level';
}

{
    my $o = (C.new but N) but P;
    is $o.^name, 'C+{N}+{P}', 'but on an instance composes a real mixin type';
    is "{$o.m} {$o.mk()()} {$o.pm} {$o.pmk()()}", '3 3 p p',
        'but: each closure reads its own role level';
    $o.set()(9);
    is "{$o.m} {$o.mk()()} {$o.pm}", '9 9 p', 'but: a write through the closure reaches that level';
}

{
    my $o = C.new but N;
    $o.set()(5);
    is $o.mk()(), 5, 'a single mixed-in role: the closure reads the written value';
}
