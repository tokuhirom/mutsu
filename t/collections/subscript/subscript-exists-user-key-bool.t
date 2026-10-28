use Test;

plan 6;

# `:exists` asks the answer of a user EXISTS-KEY for its Bool, so
# an answer that is an object runs its own `Bool` (Red's AST node throws a
# control exception from it to learn what a grep block tests).

my @asked;
class Answer {
    has $.truth;
    method Bool { @asked.push: "Bool"; $!truth }
}
class Keyed {
    method EXISTS-KEY(\k) { Answer.new(truth => k eq "yes") }
}
my $k = Keyed.new;

is $k<yes>:exists, True, 'an object answer is coerced through its Bool (true)';
is $k<no>:exists, False, '... (false)';
is $k<yes>:!exists, False, ':!exists negates the coerced answer';
is $k{"no"}:!exists, True, ':!exists on a false answer';
is @asked.elems, 4, 'the answer object\'s Bool ran once per subscript';

class Throwing {
    method EXISTS-KEY(\k) { class { method Bool { die "from-bool" } }.new }
}
throws-like { Throwing.new<a>:exists }, X::AdHoc, message => /from\-bool/,
    'a die inside the answer\'s Bool unwinds the :exists statement';
