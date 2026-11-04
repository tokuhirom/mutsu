use Test;

plan 5;

# `my ::T $x` outside a signature binds no capture: rakudo types the variable
# `NQPMu`, which has no `Mu` methods (#15516).

lives-ok { EVAL 'my ::a $a' }, 'declaring a ::a variable lives';
lives-ok { EVAL 'my ::foo $x; 1' }, 'declaring and leaving it unused lives';
throws-like 'my ::foo $x, say $x', Exception, 'printing it dies (no gist on NQPMu)';
throws-like 'my ::foo $x = 5', X::TypeCheck::Assignment, 'assigning to it fails the type check';
throws-like 'my ::foo $x; $x.bar', X::Method::NotFound, 'it answers no Mu method';
