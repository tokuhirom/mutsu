use lib 't/lib';
use Test;
use CallerBlockPackage;

# A block runs in the package it was *declared* in, whoever calls it. The
# closure body already restored its declaring package on entry, but skipped the
# restore when that package was `GLOBAL` — so a file-scope block invoked from
# inside a module kept the *module's* package, and anything it declared was
# named after the module.
#
# `Test::Util`'s `group-of` invokes the block it is handed, so
# `group-of N => 'desc' => { my class Foo {} }` named the class `Test::Util::Foo`
# and every error message quoting it diverged from rakudo's
# (roast/integration/error-reporting.t, "X::Multi::NoMatch correct shows named
# arguments").

plan 4;

call-it {
    my class Inner {}
    is Inner.^name, 'Inner', 'a file-scope block declares in GLOBAL, not the callee package';
    is Inner.new.gist, 'Inner.new', 'and renders under that name';
}

# The reverse still holds: a block declared inside a package keeps that package
# even when a foreign frame invokes it.
module Outer {
    our sub make-block { -> { my class Nested {}; Nested.^name } }
}
is call-it(Outer::make-block()), 'Outer::Nested',
    'a block declared in a package still declares in that package';

# And a plain local call is unaffected.
sub local-call(&blk) { blk() }
is local-call({ my class Plain {}; Plain.^name }), 'Plain',
    'a same-package call is unchanged';
