use Test;
use lib 't/fixtures/named-sub-expression-shadow/lib';

# Found via JSON::Native: `sub EXPORT` builds a wrapper with a named sub
# expression (`sub to-json(...) { ... }`) in a file that has its own file-level
# `to-json`. The expression is the nested routine's own closure, not whatever a
# by-name read finds.

plan 4;

use NamedSubExprShadow;

is wrapped()(1), 'wrapped:pretty=False', 'a named sub expression stored in a hash is its own closure';
is plain()(7), 'plain:7', 'a named sub expression assigned to a scalar is its own closure';
is to-json(1), 'orig:pretty=False', 'the EXPORT map hands out the wrapper, which calls the original';
is to-json(1, :sorted-keys), 'orig:pretty=False,sorted-keys=True', 'adverbs still pass through the wrapper';
