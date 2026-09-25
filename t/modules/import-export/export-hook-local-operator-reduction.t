use Test;

# An operator declared as a plain local routine inside `sub EXPORT` (no
# `is export`) and exported through the returned Map must be known to the
# importer's parser as a DECLARED operator: plain infix use parsed anyway
# through the speculative infix-word matcher, but the reduction metaop `[__]`
# was rejected (Understitch's `[_] 'aa' .. 'bb'`). Its `is equiv` trait must
# also reach the importer's parse.

plan 5;

use lib 't/lib';
use ExportHookReduceOp;

is "a" __ "b", "a b", 'EXPORT-hook operator works as an infix';
is ([__] 'aa' .. 'ac'), 'aa ab ac', 'reduction metaop over the EXPORT-hook operator';
is ([__] 1, 2), '1 2', 'parenthesized reduction in call-argument position';
is (1 __ 2 + 3), '1 5', 'is equiv(&infix:<~>) binds looser than additive';
is ([\__] 1, 2, 3).List, ('1', '1 2', '1 2 3'), 'triangular reduction';
