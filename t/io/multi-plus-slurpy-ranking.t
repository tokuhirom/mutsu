use v6;
use Test;

# From uniname-words: a `+@w` candidate that the single-argument rule lets bind
# a non-list argument stays in rakudo's narrowness relation. With a required
# named candidate of the same positional type present, `f("x")` must still pick
# the plain typed candidate, whatever order they are declared in.

plan 9;

{
    my proto sub f(|) {*}
    my multi sub f(Str:D $w) { "A" }
    my multi sub f(+@w, :$any) { "B" }
    my multi sub f(Str:D $W, :$partial!) { "C" }
    is f("x"), "A", 'typed candidate beats +@ slurpy next to a required-named one';
    is f("x", :partial), "C", 'required named selects its candidate';
    is f("x", "y"), "B", 'two arguments reach the slurpy';
}

{
    my proto sub f(|) {*}
    my multi sub f(+@w, :$any) { "B" }
    my multi sub f(Str:D $W, :$partial!) { "C" }
    my multi sub f(Str:D $w) { "A" }
    is f("x"), "A", 'declaration order does not matter';
}

{
    my proto sub f(|) {*}
    my multi sub f(Str:D $w) { "A" }
    my multi sub f(+@w) { "B" }
    my multi sub f(Str:D $W, :$partial!) { "C" }
    is f("x"), "A", 'without a named parameter on the slurpy';
    is f(<a b>), "B", 'a list argument binds the slurpy';
}

{
    my proto sub f(|) {*}
    my multi sub f(Str:D $w) { "A" }
    my multi sub f(*@w, :$any) { "B" }
    my multi sub f(Str:D $W, :$partial!) { "C" }
    is f("x"), "A", '*@ slurpy behaves the same';
}

{
    my proto sub f(|) {*}
    my multi sub f(Str:D $w) { "A" }
    my multi sub f(+@w, :$any) { "B" }
    my multi sub f(Str:D $W, :$partial) { "C" }
    is f("x"), "C", 'an optional named candidate is narrower than the plain one';
    is f(5), "B", 'a non-Str argument reaches the slurpy';
}
