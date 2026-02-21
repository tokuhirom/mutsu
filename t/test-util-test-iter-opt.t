use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 2;

ok &test-iter-opt.defined, 'test-iter-opt is exported';

class Iterator {};
class EmptyIter is Iterator {
    method pull-one() { IterationEnd }
    method count-only() { 0 }
    method bool-only() { False }
}

test-iter-opt(EmptyIter.new, 0, 'test-iter-opt handles empty iterator');
