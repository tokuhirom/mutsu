use Test;
use lib $*PROGRAM.parent(2).add("packages/Test-Helpers");
use Test::Util;

# L<S10/Packages>

use lib $*PROGRAM.parent(2).add("packages/FooBarBaz/lib");
use lib $*PROGRAM.parent(2).add("packages/OverrideTest/lib");
use lib $*PROGRAM.parent(2).add("packages/LoadFromInside/lib");
use lib $*PROGRAM.parent(2).add("packages/PM6/lib");

plan *;
{
}
{
    eval-lives-ok 'use LoadFromInsideAClass',
}
{
}

# https://github.com/Raku/old-issue-tracker/issues/6335
#?rakudo.js.browser 2 skip "use lib doesn't work in the browser"
subtest '`use lib` accepts IO::Path objects' => {
    plan 2;
    constant $path = $*PROGRAM.parent(2).add('packages/Test-Helpers').absolute;
    is_run "use lib '{$path}'.IO; use Test::Util",
        {:out(''), :err(''), :0status}, 'single object';

    is_run "use lib '$path', '{$path}'.IO; use Test::Util",
        {:out(''), :err(''), :0status}, 'a mixed list of IO::Path and Str';
}

# vim: expandtab shiftwidth=4
