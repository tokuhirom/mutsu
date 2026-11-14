use Test;

# An anonymous `$` parameter is a parameter, not an anonymous `state`
# variable of the enclosing block.

plan 5;

sub f($c) { $c.map(-> $ { 1 }).List }
is f((1, 2)), (1, 1), 'a pointy block with an anonymous parameter';

sub g { sub ($) { 'x' } }
is g()(5), 'x', 'an anonymous sub with an anonymous parameter';

sub h { (1, 2, 3).map(-> $ { ++$ }).List }
is h(), (1, 2, 3), 'a bare $ in the pointy body is still its own state';
is h(), (1, 2, 3), 'and restarts with each clone of the block';

for ^1 {
    my @r = (^3).map({ ++$ });
    is @r.join(','), '1,2,3', 'a bare $ in a block still counts';
}
