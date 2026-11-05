use Test;

plan 8;

# A `.map`/`.grep` callback is run by the inline loop, which leaves out the
# captured names its body never mentions (a lexical type's own key, a `state`
# variable's metadata key). The ones the body DOES mention must still work.

sub bump-state {
    state $s = 0;
    (1..3).map({ $s += $_ }).elems;
    $s
}
is bump-state(), 6, 'a state variable the body assigns is updated';
is bump-state(), 12, 'and keeps its value across calls';

sub unrelated-state {
    state $c = 0;
    $c++;
    my @r = (1..4).map(* * 2).grep(* > 2);
    ($c, @r.join(','))
}
is-deeply unrelated-state(), (1, '4,6,8'), 'a state variable the body ignores is untouched';
is-deeply unrelated-state(), (2, '4,6,8'), 'and still counts on the next call';

my class Pt { has $.v }
is-deeply (1..3).map({ Pt.new(v => $_).v }).list, (1, 2, 3), 'a lexical class the body names resolves';
is (1..3).map({ $_ * 2 }).grep(* > 2).elems, 2, 'a body that names no lexical class runs with one in scope';

my enum Color <Red Green Blue>;
is-deeply (0..2).map({ Color($_).key }).list, ('Red', 'Green', 'Blue'), 'an enum the body names resolves';

sub inner-class {
    my class Local { method hi($x) { "hi $x" } }
    (1, 2).map({ Local.hi($_) }).list
}
is-deeply inner-class(), ('hi 1', 'hi 2'), 'a routine-local class named by the body resolves';
