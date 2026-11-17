use Test;

# A role body's `my constant` is the role body's lexical (#17833): declaring
# the role, composing it into a class, through another role or by a runtime
# mixin leaves the declaring scope's meaning of the name alone, while the
# role's methods keep reading the constant. Checked against rakudo.

plan 8;

role RB { my constant B = Mu.new }
class XB does RB { }
class B { method c { 1 } }
is B.new.c, 1, 'a later class of the same name is not shadowed by the constant';
is B.^name, 'B', 'and is the class';

role R {
    my constant K = Mu.new;
    method b { K.^name }
}
class X does R { }
my %h;
%h does R;
my $o = Mu.new but R;
role R2 does R { }
class Y does R2 { }

is X.b, 'Mu', 'a composing class reads the constant';
is %h.b, 'Mu', 'so does a runtime `does` mixin';
is $o.b, 'Mu', 'and a `but` mixin';
is Y.b, 'Mu', 'and a class composing it through another role';

my $Q = 'outer';
role S { my constant Q = 'role'; method q { Q } }
class Z does S { }
is Z.q, 'role', 'the method reads the role constant';
is $Q, 'outer', 'an outer variable of a similar name is untouched';
