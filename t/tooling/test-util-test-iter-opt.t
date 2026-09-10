use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 3;

ok &test-iter-opt.defined, 'test-iter-opt is exported';

class Iterator {};
class EmptyIter is Iterator {
    method pull-one() { IterationEnd }
    method count-only() { 0 }
    method bool-only() { False }
}

test-iter-opt(EmptyIter.new, 0, 'test-iter-opt handles empty iterator');

my $state = 0;
class OneIter is Iterator {
    method pull-one() {
        if $state < 1 {
            $state = $state + 1;
            return 0;
        }
        return IterationEnd;
    }
    method count-only() { 1 - $state }
    method bool-only() { $state < 1 }
}

test-iter-opt(OneIter.new, [0], 'test-iter-opt handles one-item iterator');
