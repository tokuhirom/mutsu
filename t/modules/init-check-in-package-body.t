use Test;

# An INIT or CHECK phaser in a class, role or package body, or in a routine
# of one, runs once before the mainline, in the unit's INIT/CHECK order --
# not when the body runs (#10552).

plan 14;

my @log;
@log.push: 'main';

class C {
    CHECK @log.push: 'c-check';
    INIT @log.push: 'c-init';
    method m { INIT @log.push: 'm-init'; 1 }
}

role R { INIT @log.push: 'r-init' }
class A does R { }
class B does R { }

CHECK @log.push: 'top-check';
INIT @log.push: 'top-init';

module M { INIT @log.push: 'mod-init' }

sub f { INIT @log.push: 'f-init' }

is-deeply @log, [<top-check c-check c-init m-init r-init top-init mod-init f-init w-init main>],
    'body phasers join the unit INIT/CHECK sequence, CHECK in reverse';

class D {
    my $x;
    INIT { $x = 5 }
    method get { $x }
}
is D.get, 5, 'an INIT in a class body sets the body lexical a method reads';

class E {
    my $x = 3;
    my $seen;
    INIT { $seen = $x }
    method seen { $seen }
}
is E.seen, Any, 'the INIT sees the body lexical in its static state';

class F {
    our $o;
    INIT $o = 7;
}
is $F::o, 7, 'an INIT in a class body sets an our variable of the class';

class G {
    sub helper { 'helped' }
    my $r;
    INIT $r = helper();
    method r { $r }
}
is G.r, 'helped', 'an INIT in a class body calls a routine of the body';

class H {
    method stamp { my $t = INIT now; $t }
}
ok H.stamp === H.stamp, 'a value-form INIT in a method is evaluated once';

class Outer {
    class Inner {
        my $y;
        INIT $y = 2;
        method y { $y }
    }
}
is Outer::Inner.y, 2, 'an INIT in a nested class body re-enters the nested class';

my ($cls, $pkg);
class N {
    INIT $cls = $?CLASS.^name;
    INIT $pkg = $?PACKAGE.^name;
}
is $cls, 'N', '$?CLASS in a class-body INIT is the class';
is $pkg, 'N', '$?PACKAGE in a class-body INIT is the class';

my $role-count;
role PR[::T] { INIT $role-count++ }
class P1 does PR[Int] { }
class P2 does PR[Str] { }
is $role-count, 1, 'an INIT in a parametric role body runs once';

class W {
    has $.a;
    method m { INIT @log.push: 'w-init'; $!a }
}
is W.new(a => 1).m, 1, 'a method with a moved INIT still runs';
ok @log.first('w-init'), 'the INIT of a method that reads an attribute elsewhere ran';

my $mainline-count;
class Count {
    INIT $mainline-count++;
    method again { }
}
Count.again for ^3;
is $mainline-count, 1, 'a class-body INIT does not run again with the class';

my $before = 'unset';
my $checked;
class Ck { CHECK $checked = $before }
is $checked, Any, 'a class-body CHECK runs before any mainline assignment';
