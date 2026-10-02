use Test;

# An INIT or CHECK in a class declared inside a routine or block (in its body or
# in a method) runs once, at program start (INIT, in source order) or at the
# end of compilation (CHECK, in reverse order), not when the enclosing code
# runs the declaration, and not at all only because that code never runs
# (#10711). Each expectation below is what rakudo prints.

plan 9;

my @log;

sub never-called {
    my class K {
        INIT @log.push('body-init');
        CHECK @log.push('body-check');
        method m { INIT @log.push('method-init') }
    }
}
sub never-called-too { class PK { INIT @log.push('package-class-init') } }

INIT @log.push('top-init');
CHECK @log.push('top-check');

is-deeply @log,
    [<top-check body-check body-init method-init package-class-init top-init>].Array,
    'the phasers of classes in routines that never run join the unit sequence';

my @order;
sub declares { my class K { INIT @order.push('class') }; 1 }
INIT @order.push('after');
declares();
declares();
is-deeply @order, ['class', 'after'], 'a phaser runs once, however often the declaration runs';

my $u = 0;
sub unit-write { my class K { INIT $u = 5 }; $u }
is unit-write(), 0, 'the unit lexical\'s own initializer still runs after the INIT';

sub body-lexical {
    my class K {
        my $s = 1;
        INIT { $s = 5 }
        method m { $s }
    }
    K.m
}
is body-lexical(), 1, 'a class-body lexical gets its initializer on each run of the declaration';

my @seen;
sub body-helper { my class K { my sub helper { 'h' }; method m { INIT @seen.push(helper()) } }; 1 }
sub body-sub { my class K { sub helper { 'h2' }; INIT @seen.push(helper()) }; 1 }
sub body-static { my class K { my $s = 3; INIT @seen.push($s.defined ?? 'def' !! 'undef') }; 1 }
sub nested-classes {
    my class Outer {
        my class Inner { INIT @seen.push('inner') }
        INIT @seen.push('outer');
    }
    1
}
sub with-attribute {
    my class K { has $.v = 1; INIT @seen.push('attr'); method m { $!v } }
    K.new.m
}
sub in-block { if True { my class K { INIT @seen.push('in-block') }; 1 } }

is-deeply @seen, [<h h2 undef inner outer attr in-block>].Array,
    'a phaser calls a routine of its class body, sees a body lexical in its static state, and a nested class, an attribute and a block all run at start';
is with-attribute(), 1, 'a class with an attribute still works';

# A routine declared in a method of a class, called by an INIT of that method.
class C { method m { sub helper { 5 }; my $z; INIT $z = helper(); $z } }
is C.m, 5, 'a sub declared in a method of a package class';

role R { method m { sub helper { 6 }; my $z; INIT $z = helper(); $z } }
class UR does R { }
is UR.m, 6, 'a sub declared in a method of a role';

class D { sub clshelper { 8 }; method m { my $z; INIT $z = clshelper(); $z } }
is D.m, 8, 'a sub declared in the body of a package class';
