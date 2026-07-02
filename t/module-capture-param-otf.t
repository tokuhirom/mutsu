use lib 't/lib';
use Test;

# A module-imported sub with a *capture* parameter (`|c` / `|c($a, $b)`) used to
# be forced onto the tree-walk interpreter fallback: the module-single OTF gate
# rejected any sigilless parameter, and a capture is sigilless. But a capture
# binds the argument list read-only (its sub-signature only destructures that
# capture) -- unlike a sigilless *scalar* (`\x`), it never alias-writes back to
# the caller -- so it OTF-compiles exactly as it does at top level. This test
# pins that captures in every form dispatch correctly when imported. (A plain
# `code_signature` callback param -- `&cb:(Int)` -- has been OTF-compilable since
# the earlier code-signature work; captures are the remaining sub-signature case.)

plan 5;

use CaptureParamOtf;

is cap-bare(1, 2, 3, 4),            10,   'bare capture |c binds the whole arg list';
is cap-pos(10, 3),                  7,    'capture with positional subsignature |c($a,$b)';
is cap-named(x => 'a', y => 'b'),   'ab', 'capture with named subsignature |c(:$x,:$y)';
is cap-forward(6, 7),               42,   'capture forwarded with |c to another sub';
is apply-int(-> Int $x { $x * 10 }), 100, 'code-signature callback param &cb:(Int)';
