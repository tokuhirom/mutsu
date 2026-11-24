use v6;
use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

plan 3;

# A module's CONTROL handler runs at the raise site, but its routine names are
# the module's: a control exception raised by code from another compunit must
# not hide the module's own file-scope or `our` routines (#19176; Red's
# `row-predicate` catches `CX::Red::Bool` from a policy method declared in the
# test file and calls its file-private `row-expression-key`).
use ControlHandlerUnit::Policy;

is-deeply with-private-handler({ warn 'a'; warn 'b' }), ['private(a)', 'private(b)'],
    'a handler calls its module-private routine for a warning raised here';
is-deeply with-package-handler({ warn 'c' }), ['package(c)'],
    'a handler calls its module `our` routine for a warning raised here';
nok MY::<&private-key>:exists, 'the program itself does not see the helper';
