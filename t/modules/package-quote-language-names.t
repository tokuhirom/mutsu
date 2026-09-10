use Test;

plan 20;

module Q {
    our sub f() { 'module Q' }
}
module q {
    our sub f() { 'module q' }
}
module qq {
    our sub f() { 'module qq' }
}
module m {
    our sub f() { 'module m' }
}

is Q::f(), 'module Q', 'Q can name a module';
is q::f(), 'module q', 'q can name a module';
is qq::f(), 'module qq', 'qq can name a module';
is m::f(), 'module m', 'm can name a module';

class Q::Class {
    our sub f() { 'class Q' }
}
class q::Class {
    our sub f() { 'class q' }
}
class qq::Class {
    our sub f() { 'class qq' }
}
class m::Class {
    our sub f() { 'class m' }
}

is Q::Class::f(), 'class Q', 'Q can start a class name';
is q::Class::f(), 'class q', 'q can start a class name';
is qq::Class::f(), 'class qq', 'qq can start a class name';
is m::Class::f(), 'class m', 'm can start a class name';

package Q::Package {
    our sub f() { 'package Q' }
}
package q::Package {
    our sub f() { 'package q' }
}
package qq::Package {
    our sub f() { 'package qq' }
}
package m::Package {
    our sub f() { 'package m' }
}

is Q::Package::f(), 'package Q', 'Q can start a package name';
is q::Package::f(), 'package q', 'q can start a package name';
is qq::Package::f(), 'package qq', 'qq can start a package name';
is m::Package::f(), 'package m', 'm can start a package name';

role Q::Role { }
role q::Role { }
role qq::Role { }
role m::Role { }

ok Q::Role.^name, 'Q can start a role name';
ok q::Role.^name, 'q can start a role name';
ok qq::Role.^name, 'qq can start a role name';
ok m::Role.^name, 'm can start a role name';

grammar Q::Grammar { }
grammar q::Grammar { }
grammar qq::Grammar { }
grammar m::Grammar { }

ok Q::Grammar.^name, 'Q can start a grammar name';
ok q::Grammar.^name, 'q can start a grammar name';
ok qq::Grammar.^name, 'qq can start a grammar name';
ok m::Grammar.^name, 'm can start a grammar name';
