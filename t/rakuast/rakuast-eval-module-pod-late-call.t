use Test;
use lib 't/lib';
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

plan 4;

# The imported routine keeps its module's document while the EVAL carrier
# owns a separate document.
is EVAL(q[use PodLateCall; late()]), 2,
    'string EVAL calls an imported routine with its module Pod';
is EVAL(Q[use PodLateCall; late()].AST), 2,
    'AST EVAL calls an imported routine with its module Pod';
is EVAL(q[=begin pod
own EVAL document
=end pod
sub own { $=pod.elems }; own()]), 1,
    'a routine declared in EVAL reads the EVAL document';
is $=pod.elems, 0, 'the caller document remains separate';
