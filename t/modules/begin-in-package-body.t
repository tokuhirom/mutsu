use Test;

# A BEGIN written in a class, role or package body, or in a routine of one
# (a method included), runs once at BEGIN time, in source order, ahead of the
# mainline: not when the body or the routine runs, and not at all because that
# code never runs (#10328). Each expectation below is what rakudo prints.

plan 9;

my @log;
BEGIN @log.push('top1');

class A {
    BEGIN @log.push('A-body');
    method m { BEGIN @log.push('A-method'); 1 }
    multi method mm(Int $x) { BEGIN @log.push('A-multi'); 1 }
    submethod BUILD { BEGIN @log.push('A-submethod') }
    method v { my $v = BEGIN { @log.push('A-value'); 42 }; $v }
    sub f { BEGIN @log.push('A-sub') }
    class Inner { method i { BEGIN @log.push('A-inner-method') } }
}

role R {
    BEGIN @log.push('R-body');
    method r { BEGIN @log.push('R-method') }
}
class C does R { }

package P { sub g { BEGIN @log.push('P-sub') } }
module M { sub h { BEGIN @log.push('M-sub') } }

sub lexical-class { my class K { method m { BEGIN @log.push('K-method') } }; K.m }

BEGIN @log.push('top2');
@log.push('mainline');

is-deeply @log,
    [<top1 A-body A-method A-multi A-submethod A-value A-sub A-inner-method
      R-body R-method P-sub M-sub K-method top2 mainline>].Array,
    'every BEGIN ran at BEGIN time, in source order, ahead of the mainline';

class Static {
    my $x = 5;
    my $seen;
    BEGIN { $seen = $x.defined }
    method seen { $seen }
}
is Static.seen, False, 'a class-body BEGIN sees a body lexical in its static state';

class Local {
    method n { my $z; BEGIN { $z = 5 }; $z }
    method o { my $z = 3; BEGIN { $z = 5 }; $z }
    method v { my $v = BEGIN { 42 }; $v }
}
is Local.n, 5, 'a BEGIN write to a method lexical is what the method starts from';
is Local.o, 3, 'the lexical\'s own initializer still runs on each call';
is Local.v, 42, 'a value-form BEGIN in a method is its value';
is Local.v, 42, 'and the same on a second call';

my $class-name;
class Named { method m { my $z = 1; BEGIN { $class-name = $?CLASS.^name } } }
is $class-name, 'Named', '$?CLASS is the class in a method\'s BEGIN';

# The BEGIN of a class body may change the class: it runs while the class is
# being declared, ahead of the methods that rely on the change.
role Greeter { method !greeting { 'hello' } }
class Talker {
    BEGIN { ::?CLASS.^add_role(::('Greeter')) }
    method hi { self!greeting }
}
is Talker.hi, 'hello', 'a class-body BEGIN that composes a role runs before the class is composed';

class Plain { EVAL q[has $.w] }
throws-like { Plain.new(w => 1).w }, X::Method::NotFound,
    'a run-time EVAL in a class body that is declared early still does not declare an attribute';
