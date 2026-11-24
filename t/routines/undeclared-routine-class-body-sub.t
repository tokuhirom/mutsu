use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# A `sub` declared in a class or package body is callable from that body's
# own statements. The BEGIN prologue moves those statements out of the
# declaration, and the undeclared-routine check now scans them in the
# declaration's scope instead of a scope of their own. A `use` in a
# `unit class` used to hide the bug, because it made the check skip the
# whole unit; since #13441 it no longer does (Timezones::ZoneInfo).

plan 6;

is_run 'class C { sub f($y) { "called $y" }; say f(1) }',
    { :out("called 1\n"), :err(''), :status(0) },
    'a class body calls its own sub';

is_run "unit class C;\nsub f(\\y) \{ y * 2 }\nsay f(21);",
    { :out("42\n"), :err(''), :status(0) },
    'a unit class body calls its own sub';

is_run "unit class C;\nuse Test;\nsub f(\$y) \{ 'ok' }\nsay f(1);",
    { :out("ok\n"), :err(''), :status(0) },
    '... also with a use in the unit class';

is_run 'package P { sub f { "pkg" }; say f() }',
    { :out("pkg\n"), :err(''), :status(0) },
    'a package body calls its own sub';

is_run 'class C { sub f { 1 } }; f()',
    { :out(''), :err(/'Undeclared routine'/), :status({ $_ != 0 }) },
    'the class sub is still not callable outside the class';

is_run 'class C { nosuch15861() }',
    { :out(''), :err(/nosuch15861/), :status({ $_ != 0 }) },
    'an undeclared call in a class body is still reported';
