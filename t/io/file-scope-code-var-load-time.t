use lib 't/lib';
use Test;

# A module's file-scope `my &f` that a routine assigns at load time keeps the
# assignment: the module body, and its later routines, see the code object.
plan 2;

use CodeVarLoadTime;

is body-report(), 'True f', '&f is defined and callable in the module body after load()';
is run(), 'f/g1', 'an exported routine reads both reassigned code variables';
