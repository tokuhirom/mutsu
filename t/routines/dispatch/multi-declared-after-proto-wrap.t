use Test;

# #15925: a `multi` declared after `&proto.wrap(...)` (or after `&f` was
# captured) is a compile-time declaration in rakudo, so the wrapped /
# captured dispatch sees it.

plan 6;

proto sub f(|) {*}
multi sub f(Int $x) { "int $x" }
multi sub f(Str $x) { "str $x" }
&f.wrap(-> |c { "wrapped(" ~ callsame() ~ ")" });
is f(1), 'wrapped(int 1)', 'wrapped call, candidate declared before';
multi sub f(Num $x) { "num $x" }
is f(1e0), 'wrapped(num 1)', 'wrapped call reaches a candidate declared after .wrap';
is f("s"), 'wrapped(str s)', 'earlier candidates still reachable';

proto sub p(|) {*}
multi sub p(Int $x) { "p-int" }
my &g = &p;
multi sub p(Str $x) { "p-str" }
is g(1), 'p-int', 'a captured dispatcher: candidate declared before';
is g("a"), 'p-str', 'a captured dispatcher reaches a candidate declared after';

{
    proto sub h(|) {*}
    multi sub h(Int $x) { "h-int" }
    my &k = &h;
    is k(2), 'h-int', 'a block-local family is unaffected';
}
