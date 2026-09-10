use Test;
use lib 't/lib';
use TestAssertOtf;

# §D multi-dispatch VM-ization: an imported `is test-assertion` sub now takes the
# same parse path as a locally-declared assertion helper (the parser registers it
# as test-assertion), which routes it through the OTF-compilable dispatch path
# instead of forcing it onto the interpreter fallback. These check that imported
# assertion helpers stay byte-identical end-to-end (pass/fail counts), including
# the default-parameter case.

plan 7;

# 1-3. cross-module test-assertion helper works end-to-end (OTF-compiled path).
assert-eq(1, 1, 'ints equal');
assert-eq([1, 2, 3], [1, 2, 3], 'arrays eqv');
assert-eq("x", "x", 'strings equal');

# 4-5. default-parameter test-assertion helper, omitted vs provided.
assert-truthy(42);
assert-truthy("yes", "explicit description");

# 6-7. the helper still composes inside a passing program (no spurious failures).
assert-eq(({ 1 + 1 })(), 2, 'block result');
assert-truthy(caller-line() > 0, 'caller-line returns a positive line');
