use Test;
use lib 'roast/packages/S11-modules/lib';

# `require "GH2983.rakumod"` runs the file's top level in GLOBAL: a package-less
# file's types keep their own names instead of gaining a guessed `GH2983::`
# prefix (#14336). Rakudo runs the file again too, so the two types are
# distinct, but they are named alike.
plan 3;

my $first;
{
    require GH2983 <C-GH2983>;
    $first = C-GH2983.^name;
}
is $first, 'C-GH2983', 'require by module name: the type has its own name';

my $second;
{
    require ("GH2983.rakumod") <C-GH2983>;
    $second = C-GH2983.^name;
}
is $second, 'C-GH2983', 'require by path: the type has its own name too';

my $role;
{
    require ("GH2983.rakumod") <R-GH2983>;
    $role = R-GH2983.^name;
}
is $role, 'R-GH2983', 'a role loaded by path has its own name';
