use Test;

# #18684: a named capture parameter takes traits (`|c is raw`), and a
# sigilless or capture parameter, raw by construction, renders no explicit
# `is raw`.

plan 7;

is :(|c is raw).raku, ':(|c)', 'a named capture takes is raw';
sub f(|c is raw) { c.raku }
is f(1, :a), '\\(1, :a)', 'a routine with |c is raw binds its capture';
sub g(|c is raw where { .elems > 1 }) { c.elems }
is g(1, 2), 2, 'is raw then a where clause';
is :(| is raw).raku, ':(|)', 'an anonymous capture with is raw';
is :(\x is raw).raku, ':(\\x)', 'a sigilless parameter renders no is raw';
is :(Int \x is raw).raku, ':(Int \\x)', '... with a type';
is :($x is raw).raku, ':($x is raw)', 'a sigiled parameter keeps is raw';
