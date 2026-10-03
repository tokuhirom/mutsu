use Test;

plan 3;

# A role whose named parameter holds a type object still recognizes its own
# pun as `::?CLASS`, so a `::?CLASS:U` invocant constraint dispatches.
class B { }
role R[::V = Any, :$g = B] {
    multi method f(::?CLASS:U: Str $s) { "U $s" }
    method same { self ~~ ::?CLASS }
}
is R.f('x'), 'U x', '::?CLASS:U invocant on the pun';
ok R.same, 'the pun smartmatches its own ::?CLASS';
ok R[Any, :g(B)] ~~ R[Any, :g(B)], 'a named type-object argument matches itself';
