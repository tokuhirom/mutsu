use Test;
plan 6;

# `wrapped_sub_is_the_callee` asks whether any multi candidate answers a name
# through the base-name index (#15498); a candidate added or removed after the
# index was filled must still be seen.
proto sub f(|) {*}
multi sub f(Int $x) { "int $x" }
multi sub f(Str $x) { "str $x" }
&f.wrap(-> |c { "wrapped(" ~ callsame() ~ ")" });
is f(1), 'wrapped(int 1)', 'a wrapped multi runs its wrapper';
is f("a"), 'wrapped(str a)', 'for every candidate';

multi sub f(Num $x) { "num $x" }
is f(1e0), 'wrapped(num 1)', 'a candidate declared after the first calls is wrapped too';

{
    my sub g($x) { "inner $x" }
    is g(2), 'inner 2', 'a lexical sub in a program with a wrapped multi';
}
sub h($x) { "h$x" }
&h.wrap(-> $x { "w" ~ callsame() });
is h(3), 'wh3', 'a wrapped plain sub';
is f("b"), 'wrapped(str b)', 'and the multi is unaffected by it';
