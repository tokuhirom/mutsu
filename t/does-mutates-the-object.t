use Test;

plan 22;

# Raku's `does` mixes a role into *the object*, so every reference to it sees
# the mixin (`but` is the copying one). The object is reblessed into the mixin
# type `C+{R}`.

role Marker { method mark { 'marked' } }
class C { has $.n = 1 }

{
    my $x = C.new;
    my $y = $x;
    $y does Marker;
    ok $y ~~ Marker, 'the variable does was written on sees the role';
    ok $x ~~ Marker, 'an alias of the same object sees the role too';
    ok $x === $y, 'does does not replace the object';
    is $x.^name, 'C+{Marker}', 'the object is reblessed into the mixin type';
    is $x.mark, 'marked', 'the role method is callable through the alias';
    is $x.n, 1, 'the class attribute survived the mixin';
}

{
    sub apply($p) { $p does Marker }
    my $c = C.new;
    apply($c);
    ok $c ~~ Marker, 'a mixin applied inside a routine reaches the caller';
}

{
    my $x = C.new;
    my $y = $x but Marker;
    ok $y ~~ Marker, 'but composes the role on its result';
    nok $x ~~ Marker, 'but leaves the original object alone';
}

{
    my $x = C.new;
    $x does Marker;
    is $x.^parents.map(*.^name).join(','), 'C', 'the mixin type inherits from the class';
    ok $x.^roles.map(*.^name).grep('Marker').Bool, 'the mixin type composes the role';
    ok $x ~~ C, 'the object still matches its original type';
    ok $x.isa(C), 'and still isa the original class';
    nok $x.isa(Marker), 'but does not isa the role';
}

# Two roles declaring the same method: applying them one at a time is legal
# (the later one wins), even though composing them side by side would be a
# X::Role::Composition::Conflict.
{
    role A { method who { 'A' } }
    role B { method who { 'B' } }
    my $x = C.new;
    $x does A;
    is $x.who, 'A', 'first mixin provides the method';
    $x does B;
    is $x.who, 'B', 'a second mixin overrides it';
    is $x.^name, 'C+{A}+{B}', 'each mixin stacks a type on the previous one';
    ok $x ~~ A && $x ~~ B, 'the object does both roles';
}

# A submethod is not inherited, but the mixin type IS the object's own class, so
# a role's submethod is callable on it. (File::Temp mixes in a role whose
# `submethod DESTROY` unlinks the temporary file.)
{
    role Closer { submethod shut { 'shut' } }
    my $x = C.new;
    $x does Closer;
    is $x.shut, 'shut', 'a role submethod is callable on the mixed-into object';

    class Sub is C { }
    my $s = Sub.new;
    $s does Closer;
    is $s.shut, 'shut', 'and on a subclass instance too';
}

# A role attribute is seeded on the already-constructed object.
{
    role Counted { has $.count is rw = 7 }
    my $x = C.new;
    $x does Counted;
    is $x.count, 7, 'a mixed-in role attribute gets its default';
    $x.count = 9;
    is $x.count, 9, 'and is writable';
}
