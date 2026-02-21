use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 1;

ok &test-iter-opt.defined, 'test-iter-opt is exported';
