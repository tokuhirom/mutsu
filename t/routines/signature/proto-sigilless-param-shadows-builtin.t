use Test;

# A proto body sees its sigilless parameters as terms, also when the name is a
# builtin routine (`item`), as a plain sub body does (#16933).

plan 4;

proto f(\item, *@p) { "{item}-{@p}" }
multi f(\item, *@p) { "multi" }
is f(5, 1), '5-1', 'sigilless param named like a builtin in a proto body';

proto g(\item) { item }
multi g(\item) { item }
is g(7), 7, 'bare read of the parameter';

proto h(\thing) { thing }
multi h(\thing) { thing }
is h(8), 8, 'an ordinary sigilless name';

sub k(\item) { item }
is k(3), 3, 'plain sub';
