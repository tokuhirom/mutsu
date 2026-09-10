use Test;

plan 10;

# A `-->` return constraint must be the LAST element of a signature. Anything
# after it (whether comma- or semicolon-separated) is a stray parameter and
# must throw X::Syntax::Malformed (return constraints only allowed at the end).
# See roast/S32-exceptions/misc.t (old-issue-tracker #4968).
sub bad($code) { throws-like $code, X::Syntax::Malformed, :what(/return/) }
bad 'sub foo (--> Bool Int $x, Int $y) { True }';
bad 'sub foo (--> Bool Int $x; Int $y) { True }';
bad 'sub foo (--> Bool Int $x, Int $y)';
bad 'sub foo (--> Bool Int $x; Int $y)';
bad 'sub foo (--> Bool, Int $x, Int $y)';
bad 'sub foo (--> Bool; Int $x; Int $y)';
bad 'sub foo ($x, --> Bool, Int $y)';
bad 'sub foo ($x; --> Bool; Int $y)';

# Legal signatures with a trailing return constraint, and multidimensional
# signatures using `;`, must remain unaffected.
{
    sub good(--> Int) { 42 }
    is good(), 42, 'trailing --> return constraint still works';
}
{
    sub multidim(@a; @b) { @a.elems + @b.elems }
    is multidim([1, 2], [3, 4, 5]), 5, 'multidimensional `;` signature still works';
}
