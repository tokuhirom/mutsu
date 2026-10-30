use Test;

# ADR-14843: `::?CLASS` and `::?ROLE` are the frame record's, so a role method
# composed into several classes answers the composing class, and a role body
# mixed into a value sees the value's base type. Expected values are raku's.

plan 10;

role Named {
    method role-name { ::?ROLE.^name }
    method class-name { ::?CLASS.^name }
    method self-class { self.^name }
}
role Wrapper {
    method inner-role { Named.role-name }
}
class A does Named { method own { ::?CLASS.^name } }
class B does Named { }
class C is A { }

is A.new.role-name, 'Named', '::?ROLE in a composed method';
is A.new.class-name, 'A', '::?CLASS is the composing class (A)';
is B.new.class-name, 'B', '::?CLASS is the composing class (B)';
is C.new.class-name, 'A', 'an inherited role method keeps its composing class';
is C.new.self-class, 'C', 'while self is the runtime type';
is C.new.own, 'A', 'a class method reached through a subclass';

# nested calls do not leak one frame's record into the next
class Outer {
    method m { A.new.class-name ~ '/' ~ ::?CLASS.^name ~ '/' ~ B.new.class-name }
}
is Outer.new.m, 'A/Outer/B', 'a callee frame restores the caller record';

# a role body mixed into a value
role Tagged { method tag { ::?ROLE.^name ~ ':' ~ self.^name } }
my $x = 5 but Tagged;
is $x.tag, 'Tagged:Int+{Tagged}', 'a role mixed into a value';

# does at run time
class D { }
my $d = D.new but Named;
is $d.role-name, 'Named', '::?ROLE through a mixin';
is-deeply (A.new.class-name, B.new.class-name, A.new.class-name),
          ('A', 'B', 'A'), 'repeated composed calls alternate correctly';
