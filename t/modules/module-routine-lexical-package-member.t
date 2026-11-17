use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

# A module routine names a member of its module's own lexical (`my`)
# package, class or module by qualified name: an enum member and an
# `our constant` resolve when the routine is called from the program, after
# the module's scope is gone (#17720). Every expectation was checked against
# rakudo.

use LexicalPackageMembers;

plan 10;

is enum-member(), 'Bad', "an enum member of the module's `my class`";
is enum-member().value, -1, 'the enum member keeps its value';
is enum-type().^name, 'Status', "the enum type of the module's `my class`";
is package-constant(), 5, "an `our constant` of a `my package` with no types";
is class-constant(), 'k', "an `our constant` of a `my class`";
is class-enum-member(), 'y', "an enum member of the `my class`";
is class-enum-member().value, 1, 'its value';
is nested-package-constant(), 'd', 'a constant of a package nested in a `my package`';
is module-enum-member().Int, 1, "an enum member of a `my module`";
ok enum-member-smartmatch(), 'a rebuilt enum member still matches its enum';
