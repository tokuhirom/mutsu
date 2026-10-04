use Test;

# From the IP::Addr distribution: a `method TOP` that delegates to a rule
# (`self.rule`) returns that rule's Match, so that rule's action must fire;
# and a lone Pair given as `:args(:validate)` passes the start rule no args.

plan 4;

grammar G {
    method TOP (Bool :$validate = False) { self.v }
    rule v { <d> }
    token d { \d+ }
}
class A {
    method v($m) { $m.make([ 'v', $m<d>.ast ]) }
    method d($m) { $m.make($m.Int) }
}

my $m = G.parse("12", :actions(A.new));
is-deeply $m.ast, ['v', 12], "action of the delegated rule fires";

$m = G.parse("12", :actions(A.new), :args(:validate));
is-deeply $m.ast, ['v', 12], "a lone Pair :args is accepted";

grammar H {
    method TOP (Bool :$validate = False) { $*V = $validate; self.d }
    token d { \d+ }
}
{
    my $*V;
    H.parse("5", :args(:validate));
    is $*V, False, "a lone Pair :args passes no arguments";
    H.parse("5", :args(\(:validate)));
    is $*V, True, "a Capture :args still passes the named argument";
}
