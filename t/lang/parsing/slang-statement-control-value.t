use lib 't/lib';
use Test;

# Checked against rakudo (RakuAST frontend). A slang production that re-runs its actions in the unit (ADR-17243) stands
# for the replay call and its own statement (#17460, Qwiratry's `givenroot` and
# `mold`):
# - a production that attaches an expression keeps it as the statement's value,
#   so it can be a routine's return value;
# - a `my`-scoped method a `routine-declarator` builds from `$*BLOCK` answers its
#   `meta-object` with the method object (it is in no class's method table).

plan 5;

use StatementValueSlang;

sub elems-of($data) {
    withroot $data {
        root().elems
    }
}
is elems-of([1, 2, 3]), 3, 'an expression production is its routine\'s return value';

sub nested($outer, $inner) {
    my $value;
    withroot $outer {
        withroot $inner {
            $value = root()[1];
        }
    }
    $value
}
is nested(['outer'], ['inner', 'value']), 'value', 'nested productions';

class Tree {
    keep section do { self.^name }
}

my %molds = kept-molds();
ok %molds<section> ~~ Method, 'the declarator\'s meta-object is a Method';
is %molds<section>.count, 1, 'with the invocant as its one parameter';
is %molds<section>(Tree.new), 'Tree', 'and runs with self bound';
