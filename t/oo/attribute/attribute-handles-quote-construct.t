use Test;

# A `handles` trait may name its method with a quote construct
# (`handles q[aie]`, `handles Q<b>`); mutsu read only the `q` and failed with
# "Two terms in a row".

plan 3;

class A { method aie { 7 }; method b { 8 }; method c { 9 } }
class B { has A $.bar handles q[aie] }
class C { has A $.bar handles Q<b> }
class D { has A $.bar handles qq{c} }

is B.new(bar => A.new).aie, 7, 'handles q[...]';
is C.new(bar => A.new).b, 8, 'handles Q<...>';
is D.new(bar => A.new).c, 9, 'handles qq{...}';
