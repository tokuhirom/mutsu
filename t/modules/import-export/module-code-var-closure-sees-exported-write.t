use lib 't/lib';
use Test;
use ModuleCodeVarCell;

# #11051: a closure created in a module mainline over a module-level
# `my &var` sees a later write to that variable from an exported sub.

plan 9;

nok backend-registered(), 'no backend before registration';
is run-filter('md', 'x'), 'MISSING', 'mainline closure sees the undefined backend';
register-backend :handler(-> $b { "<$b>" });
ok backend-registered(), 'the exported sub sees its own write';
is run-filter('md', 'x'), '<x>', 'the mainline closure sees the write too';

is run-initial(), 'init:9', 'a code-initialized &var is read through the closure';
is initial-info(), 'True|1|init:2|init:1,init:2', '&var reads before the rebind';
set-initial(-> $y { "new:$y" });
is run-initial(), 'new:9', 'the closure sees the rebound code variable';
is initial-info(), 'True|1|new:2|new:1,new:2', '&var reads after the rebind';

{
    my &q = -> { 'q0' };
    our sub set-q(&h) { &q = &h }
    my &read-q = -> { q() };
    set-q(-> { 'q1' });
    is read-q(), 'q1', 'same-file block: an our sub write reaches the closure';
}
