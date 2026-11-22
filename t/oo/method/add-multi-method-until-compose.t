use Test;

# A candidate `^add_multi_method` adds to a class that is already composed
# stays out of dispatch until the class's next `^compose` (rakudo's
# `multi_methods_to_incorporate`, #18868).

plan 5;

class Late { multi method g(Int $x) { "int" } }
my $l = Late.new;
Late.^add_multi_method('g', my method g2(Str $x) { "str" });

throws-like { $l.g("a") }, X::Multi::NoMatch,
    'the added candidate is not dispatched before ^compose';
is $l.g(1), "int", 'the existing candidate still answers';

Late.^compose;
is $l.g("a"), "str", 'after ^compose the added candidate answers';
is $l.g(1), "int", '... beside the existing one';

class B { }
B.^add_multi_method('h', my method h1(Int $x) { "h-int" });
B.^compose;
is B.new.h(3), "h-int", 'a candidate added to a class composing for the first time';
