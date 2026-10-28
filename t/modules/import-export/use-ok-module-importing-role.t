use Test;
use lib 't/lib';

# A role imported by a module and composed with `does`, where the module is
# loaded by `use-ok` (an EVAL inside Test's sub) after the exporter was
# already loaded the same way.
plan 2;

use-ok 'UseOkRole::Cap';
use-ok 'UseOkRole::Op';
