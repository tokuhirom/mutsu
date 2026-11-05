use Test;

# #16612 (found by SQL::Abstract): a named parameter with a sub-signature that
# needs an element is not applicable to a call that does not pass that name, so
# it cannot win dispatch over a sibling candidate that does take the name.

plan 7;

multi lit(:literal($) ($payload, *@arguments), *%args) { 'literal' }
multi lit(:op(@) (Str $op where 'like'|'not-like', $l, $r, $e)) { 'like' }
is lit(|\(:op['like', 1, 2, 3])), 'like', 'a trailing sub-signature (alias form) is not applicable when absent';
is lit(:op['like', 1, 2, 3]), 'like', 'the same through a plain call';

multi plain(:$literal ($payload, *@arguments), *%args) { 'literal' }
multi plain(:op(@) ($a, $b)) { 'op' }
is plain(:op[1, 2]), 'op', 'a sub-signature on a plain named parameter';

multi opt(:$literal ($a?, $b?), *%args) { 'literal' }
multi opt(:op(@) ($a, $b)) { 'op' }
is opt(:op[1, 2]), 'literal', 'a sub-signature whose parameters are all optional stays applicable (declared first)';

multi passed(:$literal ($payload, *@arguments), *%args) { 'literal' }
multi passed(:op(@) ($a, $b)) { 'op' }
is passed(:literal(<a b c>)), 'literal', 'the named argument given: a list unpacks into the sub-signature';

multi only(:$literal ($payload, *@arguments), *%args) { 'literal' }
throws-like { only(:op(1)) }, X::Multi::NoMatch, 'with no other candidate the call has no match';

multi mixed(:$x, *%rest) { 'plain' }
multi mixed(:$literal ($a, $b)) { 'sub' }
is mixed(:x(1)), 'plain', 'a plain named candidate still takes its own name';
