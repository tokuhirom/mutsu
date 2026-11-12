use Test;

# A `my proto` with no candidates is a dispatcher value of its own: it keeps
# its signature and its added candidates once its module's load is over
# (#15371, CompUnit::Util's `push-unit-multi`). Checked against rakudo.

plan 4;

use lib $?FILE.IO.parent(2).sibling('lib').Str;
use CandidatelessProtoValue;

my $p := $CandidatelessProtoValue::proto;
ok $p.is_dispatcher, 'the proto is a dispatcher';
ok Numeric === $p.signature.params[0].type, 'it keeps its own signature';
is $p(42), 'bar-int', 'a candidate added in the module answers';
dies-ok { $p('str') }, 'and nothing else does';
