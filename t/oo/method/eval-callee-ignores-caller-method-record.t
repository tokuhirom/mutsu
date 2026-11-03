use Test;

# `self`, `$?CLASS` and `$?ROLE` are lexical to a declaration: a plain sub that
# EVALs one must not find the record of the METHOD that happened to call it
# (ADR-12529, #16099). A sub declared in a class body keeps its own class.

plan 7;

sub c1 { (try EVAL q[self.v]) // 'not visible' }
sub c2 { (try EVAL q[$?CLASS.^name]) // 'not visible' }
sub c3 { (try EVAL q[$?ROLE.^name]) // 'not visible' }

class K { method v { 'V' }; method m1 { c1() }; method m2 { c2() } }
role Rl { method m3 { c3() } }
class K2 does Rl { }

is K.new.m1, 'not visible', '`self` of the calling method';
is K.new.m2, 'not visible', '`$?CLASS` of the calling method';
is K2.new.m3, 'not visible', '`$?ROLE` of the calling method';

# The method itself, and a closure it creates, still see their own record.
class K3 {
    method v { 'V3' }
    method own { EVAL(q[self.v]) ~ EVAL(q[$?CLASS.^name]) }
    method viaclosure { my &f = -> { EVAL q[self.v] }; f() }
    sub helper { $?CLASS.^name }
    sub eval-helper { (try EVAL q[$?CLASS.^name]) // 'not visible' }
    method m { helper() ~ eval-helper() }
}
is K3.new.own, 'V3K3', 'a method EVALs its own self and class';
is K3.new.viaclosure, 'V3', 'a closure created in a method keeps its creator record';
is K3.new.m, 'K3K3', 'a sub declared in a class body is that class, from any caller';

throws-like { EVAL q[$?CLASS] }, X::Undeclared, '`$?CLASS` in a file-scope EVAL is undeclared';
