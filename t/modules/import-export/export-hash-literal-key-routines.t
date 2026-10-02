use lib 't/lib';
use Test;

# From the Object::Delayed distribution: a module that exports through
# `sub EXPORT { %EXPORT }` and names its routines as literal `&name` keys of a
# lexical `%EXPORT` must let the importer parse a listop call `delay-it { ... }`.
# Also `proto method NAME(...) {*}` as an expression (Object::Trampoline;
# its value is still Nil, see the issue linked from the PR).

use ExportHashKeyRoutines;

plan 2;

is (delay-it { 5 }), 5, 'routine named only by a %EXPORT<&name> key is a listop head';

# Parse-only: the expression form of `proto method` must not be "two terms in a row".
lives-ok { EVAL 'my constant &p = proto method handler(|) {*}; 1' },
    'proto method in expression position parses';
