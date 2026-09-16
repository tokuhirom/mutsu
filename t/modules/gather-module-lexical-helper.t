use lib 't/lib';
use Test;
use GatherModuleLexicalHelper;

# File::Find 0.2.5 calls a module-private helper from its exported find()
# routine's gather body.  That deferred body must retain its module package.
plan 1;

is gathered-double(21).List, (42,), 'a gather body reaches its module lexical helper';
