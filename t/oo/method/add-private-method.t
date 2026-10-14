use Test;

plan 5;

class C {
    method call { self!"secret"() }
    method secret { 'public' }
}
C.^add_private_method('secret', method () { 42 });
C.^compose;

is C.new.call, 42, 'a private method added with ^add_private_method is reached by self!name';
is C.new.secret, 'public', 'the public method of the same name is kept';
ok C.^private_method_table<secret>.defined, '^private_method_table sees it';
ok so(C.HOW.^can('add_private_method')), 'ClassHOW can add_private_method';

class D { }
D.^add_private_method('hidden', method () { 'x' });
D.^compose;
throws-like { D.new.hidden }, X::Method::NotFound, 'the private method is not public';
