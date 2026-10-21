use Test;

plan 4;

class I is Int { }

try { I.new(42).gcd(4) };
isa-ok $!, X::Method::NotFound, 'a missing method on an Int subclass instance is X::Method::NotFound';
is $!.typename, 'I', 'typename names the subclass';
is $!.message, "No such method 'gcd' for invocant of type 'I'", 'message names the subclass';
is I.new(42).abs, 42, 'a real Int method still answers on the payload';
