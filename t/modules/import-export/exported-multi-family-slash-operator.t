use lib $?FILE.IO.parent(3).add('lib');
use Test;

# An exported multi operator whose own name holds a `/` (`infix:<+/+>`): its
# candidates' registry keys are `Pkg::infix:<+/+>/2…`, so the family lookup
# must split at the arity `/`, not the first one (#11761). Kept apart from
# exported-multi-family-candidates.t, which is on the RakuAST frontend
# ratchet list: the RakuAST round-trip cannot yet express a user-defined
# infix call.

plan 2;

use ExportedMultiFamily;
is (1 +/+ 'x'), 'op-int-str 1 x', 'first candidate';
is ('y' +/+ 2), 'op-str-int y 2', 'second candidate';
