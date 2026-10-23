use v6;
use Test;

plan 13;

sub f(:@to!) { 1 }
sub g(:%to!) { 1 }

throws-like { f(:to(Mu)) }, X::TypeCheck::Binding::Parameter,
    "a named @ parameter rejects a Mu type object";
throws-like { f(:to(5)) }, X::TypeCheck::Binding::Parameter,
    "a named @ parameter rejects a non-Positional value";
is f(:to([1, 2])), 1, "a named @ parameter accepts an Array";
is f(:to((1, 2))), 1, "a named @ parameter accepts a List";
throws-like { g(:to(5)) }, X::TypeCheck::Binding::Parameter,
    "a named % parameter rejects a non-Associative value";
is g(:to({a => 1})), 1, "a named % parameter accepts a Hash";

nok \(:to(Mu)) ~~ :(:@to!), "Capture with a Mu named argument does not match :@to!";
nok \(:to(5)) ~~ :(:@to!), "Capture with an Int named argument does not match :@to!";
ok \(:to([1])) ~~ :(:@to!), "Capture with an Array named argument matches :@to!";
nok \(5) ~~ :(@a), "Capture with an Int positional does not match :(@a)";
ok \([1, 2]) ~~ :(@a), "Capture with an Array positional matches :(@a)";

class D {
    multi method inflate(Any $v, Any :$to) { 1 }
    multi method inflate(Str $v, :@to!) { 2 }
}
nok \(D.new, "a", :to(Mu)) ~~ D.^lookup("inflate").candidates[1].signature,
    "a candidate with :@to! is not a match for :to(Mu)";
ok \(D.new, "a", :to([1])) ~~ D.^lookup("inflate").candidates[1].signature,
    "a candidate with :@to! matches :to([1])";
