use Test;

plan 7;

class Empty7772 { }

throws-like { Empty7772.new(:name('Fido')).name }, X::Method::NotFound,
    method => 'name', typename => 'Empty7772',
    'an undeclared .name does not read an unknown named constructor argument';

throws-like { Empty7772.new.name }, X::Method::NotFound,
    method => 'name', typename => 'Empty7772',
    'an undeclared .name does not return Nil';

class WithAttr7772 { has $.a }

throws-like { WithAttr7772.new(:a(1), :name('x')).name }, X::Method::NotFound,
    method => 'name', typename => 'WithAttr7772',
    'an unrelated declared attribute does not create .name';

class Named7772 { has $.name }

is Named7772.new(:name('Fido')).name, 'Fido',
    'a declared .name attribute still provides its accessor';

is &say.name, 'say', 'Routine.name still works';
ok $*THREAD.name.defined, 'Thread.name still works';
ok $*KERNEL.name.defined && $*DISTRO.name.defined,
    'Kernel.name and Distro.name still work';
