use Test;
plan 7;
role R {
    my class Box { has $.v = 1; method twice { $!v * 2 } }
    method make(Int $n) { Box.new(v => $n) }
    method name { Box.^name }
}
class C does R { }
my $c = C.new;
is $c.make(3).twice, 6, 'a role-lexical class is reachable from a method';
is $c.make(4).twice, 8, 'and again (memoized key scan)';
is $c.name, 'R::Box', 'its name is the declared one';
role S {
    my class Box { has $.v = 10; method twice { $!v * 3 } }
    method make(Int $n) { Box.new(v => $n) }
}
class D does S { }
is D.new.make(2).twice, 6, 'a same-named lexical class of another role is its own';
is $c.make(3).twice, 6, 'the first role is unaffected after the second declaration';
my class Late { method hi { "late" } }
is Late.hi, "late", 'a class declared after the first scans resolves';
is $c.make(5).twice, 10, 'earlier lookups still agree after more registrations';
