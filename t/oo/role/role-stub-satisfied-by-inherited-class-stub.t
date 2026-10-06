use Test;
# From the W3C::DOM distribution's t/basic.t.
plan 3;
role R { method entities {...}; method foo {...} }
class A { method entities {...} }
class B is A { method foo {...} }
lives-ok { class C is B does R { } }, 'parent-class stubs satisfy role stubs by name';
lives-ok { C.new }, 'instance created';
dies-ok { EVAL 'class D is A does R { }' }, 'still dies when a required method is missing';
