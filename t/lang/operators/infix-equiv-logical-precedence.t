use v6.d;
use Test;

# From the FunctionalParsers distribution (t/12-alternatives-first-match): user
# operators declared `is equiv(&infix:<&&>)` / `is equiv(&infix:<||>)` take the
# precedence of the built-in logical operators, so `&&`-level binds tighter.

sub infix:<AND>( *@a ) is equiv( &infix:<&&> ) is assoc<right> { 'and(' ~ @a.join(',') ~ ')' }
sub infix:<OR>( *@a ) is equiv( &infix:<||> ) is assoc<list> { 'or(' ~ @a.join(',') ~ ')' }

is-deeply ('a' AND 'b' OR 'c' AND 'd' AND 'e' OR 'f' OR 'g'),
    'or(and(a,b),and(c,and(d,e)),f,g)', 'AND binds tighter than OR';
is ('a' OR 'b' AND 'c'), 'or(a,and(b,c))', 'OR then AND';
is ('a' OR 'b' OR 'c'), 'or(a,b,c)', 'list-associative OR';
is ('a' AND 'b' AND 'c'), 'and(a,and(b,c))', 'right-associative AND';
is (1 < 2 AND 3 < 4), 'and(True,True)', 'comparison binds tighter than AND';

done-testing;
