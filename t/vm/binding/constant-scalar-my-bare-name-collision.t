use Test;
use lib 't/lib';

# Regression pin: a module-level scalar `my $NAME` and a sigilless `constant NAME`
# collide by bare name (mutsu strips the `$` sigil from scalar names in the AST,
# so `$CRLF` and a sigilless `constant CRLF` share the key `CRLF`). When both a
# parent and a child class (in separate modules) declare `my $NAME`, the parent's
# leaked into the persistent mainline env; the child's own `my $NAME` was then
# never registered as a class-body static, so its methods fell back to whatever
# last wrote the bare-name global — including a same-named `constant` from a third
# module. This is exactly the HTTP::Message / HTTP::Request (`my $CRLF`) plus
# HTTP::UserAgent (`constant CRLF`) shape that turned a request's line-ending into
# the wrong value. See PLAN.md §1 B4 (bug #3).

plan 4;

use CrlfConst;
use CrlfDerived;
use CrlfBase;

is CrlfDerived.derived-crlf, "derived-crlf",
    'child class reads its OWN module-level `my $CRLF`, not a leaked `constant CRLF`';
is CrlfBase.base-crlf, "base-crlf",
    'parent class still reads its own module-level `my $CRLF`';

# Instances resolve the same way.
is CrlfDerived.new.derived-crlf, "derived-crlf",
    'child instance method reads its own `my $CRLF`';
is CrlfBase.new.base-crlf, "base-crlf",
    'parent instance method reads its own `my $CRLF`';
