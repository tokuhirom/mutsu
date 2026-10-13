use Test;

# From CSS::Module's calc() type checker: a destructuring sub-signature on a
# positional parameter is trial-bound, so a tie with a candidate that has none
# runs the first one declared instead of being ambiguous.

plan 6;

multi sub a( *@expr ) { 'fallback' }
multi sub a( %lhs, % ( :$op! ), *@rhs ) { 'binop' }
is a({ :num(1) }, { :op<+> }, { :num(2) }), 'fallback',
    'fallback declared first wins the tie';

multi sub b( %lhs, % ( :$op! ), *@rhs ) { 'binop' }
multi sub b( *@expr ) { 'fallback' }
is b({ :num(1) }, { :op<+> }, { :num(2) }), 'binop', 'binop declared first wins the tie';

multi sub c( *@expr ) { 'fallback' }
multi sub c( $lhs, $op, *@rhs ) { 'plain' }
throws-like { c(1, 2, 3) }, X::Multi::Ambiguous, 'without a sub-signature the tie stays ambiguous';

# A slurpy sub-signature is checked against the flattened list: a non-itemized
# Hash argument flattens to Pairs, which are not positional.
multi sub d( %lhs, % ( :$op! ), *@rhs (%, *@) ) { 'binop' }
multi sub d( *@expr ) { 'fallback' }
is d({ :num(1) }, { :op<+> }, { :num(2) }), 'fallback',
    'a flattened Hash does not satisfy a positional sub-signature';
is d({ :num(1) }, { :op<+> }, { :num(2) }, { :op<+> }, { :num(3) }), 'fallback',
    'also with more arguments';

my @items = { :num(2) }, { :op<+> };
multi sub e( %lhs, % ( :$op! ), *@rhs (%, *@) ) { 'binop' }
multi sub e( *@expr ) { 'fallback' }
is e({ :num(1) }, { :op<+> }, |@items), 'binop',
    'itemized Array elements keep their Hash, so the sub-signature matches';
