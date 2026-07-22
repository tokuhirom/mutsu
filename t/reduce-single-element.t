use v6;
use Test;

plan 12;

# A single-element reduce returns the element's *value* coerced to the
# operator's natural type — NOT `identity op elem`. Regression: `[-] 10`
# used to fold `0 - 10 = -10`.

is reduce(&infix:<->, (10,)), 10, 'reduce &infix:<-> over one element is the element';
is reduce(&infix:</>, (10,)), 10, 'reduce &infix:</> over one element is the element';
is reduce(&infix:<**>, (2,)), 2, 'reduce &infix:<**> over one element is the element';
is reduce(&infix:<%>, (7,)), 7, 'reduce &infix:<%> over one element is the element';
is reduce(&infix:<+>, (5,)), 5, 'reduce &infix:<+> over one element is the element';
is reduce(&infix:<*>, (3,)), 3, 'reduce &infix:<*> over one element is the element';

# The single element is coerced to the operator's natural type.
is reduce(&infix:<+>, ("2",)).WHAT, Int, 'numeric reduce coerces Str to Int';
is reduce(&infix:<->, ("10",)), 10, 'numeric reduce coerces "10" to 10';
is reduce(&infix:<~>, (5,)), "5", 'string reduce &infix:<~> coerces to Str';
is reduce(&infix:<~>, (5,)).WHAT, Str, '&infix:<~> single element is Str';

# Metaop reduce forms.
is ([-] 10), 10, 'metaop [-] over one element';
is ([-] 10, 3, 2), 5, 'metaop [-] still folds multiple elements left-assoc';
