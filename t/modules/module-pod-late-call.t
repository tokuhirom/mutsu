use v6;
use Test;
use lib 't/lib';

plan 3;

# A routine declared in a module reads `$=pod` as the module's own document,
# also when it is called after the module has finished loading (the loader
# restores the importer's `=pod` after the module body).
use PodLateCall;

is $PodLateCall::N, 2, '$=pod in the module body is the module document';
is late(), 2, 'a module sub called after load still sees the module document';
is $=pod.elems, 0, "the importer's own document is untouched";
