use Test;

# `my module AST {}` followed by file-scope `my class AST::Param {}`: the class
# is declared under its namespaced spelling, so `AST::Param` stays visible in
# its own compilation unit even though `AST` is a module, not a class. Only a
# `my class` written INSIDE the package body is hidden after it. From Badger.

plan 5;

my module AST { }
my class AST::Param { has $.name }
is AST::Param.new(name => 'a').name, 'a', 'namespaced lexical class under a my module';

sub make-param { AST::Param.new(name => 'b') }
is make-param().name, 'b', '... from inside a sub';

my package PKG { }
my class PKG::Item { has $.v }
is PKG::Item.new(v => 1).v, 1, '... under a my package';

module M { my class Inner { } }
nok (try M::Inner).defined, 'a my class inside a module body is still hidden';

my class Outer { }
my class Outer::Child { method hi { 'hi' } }
is Outer::Child.hi, 'hi', '... under a class, as before';
