use Test;
use experimental :rakuast;

plan 14;

# The `shape` of a declaration (`my Int %h{Str}`, `my @a[2;3]`) is placed from the
# text inside its brackets; a key type is a `Type::Simple` over a `Name` of the
# same range (#15780).
sub range-of($node) { $node.origin ?? "{$node.origin.from}-{$node.origin.to}" !! 'none' }

my $keyed = 'my Int %h{Str}'.AST.statements[0].expression;
my $shape = $keyed.shape;
isa-ok $shape, RakuAST::SemiList, 'keyed hash shape';
is range-of($shape), '10-13', 'SemiList spans the key type';
my $statement = $shape.statements[0];
is range-of($statement), '10-13', 'its statement';
is range-of($statement.expression), '10-13', 'the key Type::Simple';
is range-of($statement.expression.name), '10-13', 'and its Name';

my $multi = 'my %h{Str;Int}'.AST.statements[0].expression.shape;
is range-of($multi), '6-13', 'two key types';
is $multi.statements.map({ range-of(.expression) }).join(','), '6-9,10-13', 'each key type has its own range';

my $array = 'my @a[3]'.AST.statements[0].expression.shape;
is range-of($array), '6-7', 'a shaped array';
is range-of($array.statements[0]), '6-7', 'its dimension statement';
is range-of($array.statements[0].expression), '6-7', 'the literal keeps its range';

my $dims = 'my @b[2;3]'.AST.statements[0].expression.shape;
is range-of($dims), '6-9', 'two dimensions';
is $dims.statements.map({ range-of($_) }).join(','), '6-7,8-9', 'one range per dimension';

my $native = 'my int @c[2]'.AST.statements[0].expression.shape;
is range-of($native), '10-11', 'a native shaped array';
is range-of('my %g{Int}'.AST.statements[0].expression.shape), '6-9', 'a bare keyed hash';
