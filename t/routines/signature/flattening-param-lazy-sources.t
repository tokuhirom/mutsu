use Test;

plan 17;

# A `*@` slurpy flattens its arguments, and in Rakudo it does so lazily: a
# genuinely lazy argument (an infinite `...` sequence, a lazy `.map` pipe, an
# unbounded Range), whether passed as is or slipped with `|`, becomes a lazy
# part of the slurpy's own sequence. mutsu bound such an argument as ONE
# nested element, so `g(0, (1, 3 ... *))[^3]` was `(0 (...) (Any))` (#10888).

sub first3(*@x) { @x[^3] }
sub is-lazy-slurpy(*@x) { @x.is-lazy }

is-deeply first3(0, (1, 3 ... *)), (0, 1, 3), 'an infinite sequence argument flattens lazily';
is-deeply first3(0, |(1, 3 ... *)), (0, 1, 3), 'and so does a slipped one';
is-deeply first3(0, |(1..*)), (0, 1, 2), 'a slipped unbounded Range';
is-deeply first3(0, 1..*), (0, 1, 2), 'an unbounded Range after another argument';
is-deeply first3(0, ("a" .. *)), (0, "a", "b"), 'a Str-start unbounded Range';
is-deeply first3(|(1, 2), (5..*).map(* + 1)), (1, 2, 6), 'a lazy map pipe after a Slip';
ok is-lazy-slurpy(0, 1..*), 'the slurpy is lazy';
nok is-lazy-slurpy(1, 2), 'a slurpy of plain arguments is not';

sub plain(*@x) { @x }
is-deeply plain(1, |(2, 3), [4, 5], $[6]), [1, 2, 3, 4, 5, [6]],
    'finite arguments still flatten as before';

# Front mutation of a lazy pipe / concatenated lazy array keeps it lazy.
sub shifted(*@x) { @x.shift; @x.shift; @x[^3] }
is-deeply shifted(0, 1..*), (2, 3, 4), 'shift on a lazy slurpy';

{
    my @a = (1..*).map(* + 1);
    is @a.shift, 2, 'shift on a lazy map array returns the first element';
    @a.unshift(0);
    is-deeply @a[^3], (0, 3, 4), 'unshift puts the element in front';
    ok @a.is-lazy, 'and the array stays lazy';
    is-deeply @a.splice(1, 2), [3, 4], 'splice removes from the front part';
    is-deeply @a[^3], (0, 5, 6), 'and the rest follows';
}

{
    my @a = (1..*).map(* + 1);
    @a.shift for ^500;
    is @a[0], 502, 'many shifts in a row';
}

{
    my @a = lazy gather { take $_ for 1..* };
    @a.shift;
    is-deeply @a[^3], (2, 3, 4), 'shift on a lazy gather array';
}
