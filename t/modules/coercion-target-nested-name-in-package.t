use Test;
use lib 't/lib-coercion-nested-target';
use NestedCoerce;

# From the SQL::Abstract distribution: `Column::List(Any)` inside the module
# that declares `Column::List` must find that class's COERCE.
plan 5;

is NestedCoerce::one('a'), 'a', 'a Str is coerced through the nested-name target';
is NestedCoerce::one(<a b>), 'a,b', 'so is a list (the @list candidate)';
is NestedCoerce::with-from('t', 'x'), 't:x', 'with a leading positional';
is NestedCoerce::kind('a'), 'NestedCoerce::Column::List', 'the coerced value is the module class';
dies-ok { NestedCoerce::one() }, 'a missing argument is still an error';
