use Test;

# XML::Class: a recursive multi passes its own `$e` (declared `E $e`) on to a
# candidate whose `W $e is copy` must keep ITS declared type for assignments.
plan 3;

class E { method find { Nil } }
role W { }

multi sub d(E $e where * !~~ W, |c) { $e does W; d($e, |c) }
multi sub d(W $e is copy, Str :$n) {
    $e = $e.find;
    $e.^name;
}

is d(E.new, :n("a")), "W", "Nil assigned to an 'is copy' parameter resets to its own type";

sub copy-own(W $e is copy) { $e = Nil; $e.^name }
my $x = E.new;
$x does W;
is copy-own($x), "W", "plain call: constraint is the parameter's";
throws-like { sub f(W $e is copy) { $e = E }; f($x) }, X::TypeCheck::Assignment,
    "assignment is still checked against the parameter's own type";
