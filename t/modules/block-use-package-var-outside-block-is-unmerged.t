use Test;
use lib $?FILE.IO.parent(2).add('lib');

# A `use` inside a block merges the module's packages into that block only.
# Outside it, `$M::x` names a fresh, autovivified `GLOBAL::M::x`, not the
# module's `our $x`: it reads `(Any)`, and writing it leaves the module's
# variable alone (#13942).

plan 14;

my &show;
{
    use UseInBlockPackageVars;
    is $UseInBlockPackageVars::n, 1, 'inside the block, the qualified name is the module variable';
    is $n, 1, '... as is the imported one';
    &show = &UseInBlockPackageVars::show;
}

nok $UseInBlockPackageVars::n.defined, 'after the block, a qualified scalar is undefined';
nok @UseInBlockPackageVars::l.defined, '... and so is a qualified array';
nok %UseInBlockPackageVars::h.defined, '... and a qualified hash';
nok $GLOBAL::UseInBlockPackageVars::n.defined, '... and the GLOBAL-qualified spelling';

$UseInBlockPackageVars::n = 7;
is $UseInBlockPackageVars::n, 7, 'a qualified scalar can be assigned there';
is $GLOBAL::UseInBlockPackageVars::n, 7, '... and is the GLOBAL-qualified variable';
$UseInBlockPackageVars::n++;
is $UseInBlockPackageVars::n, 8, '... and incremented';

%UseInBlockPackageVars::h<b> = 2;
is-deeply %UseInBlockPackageVars::h, {b => 2}, 'an element store autovivifies a fresh hash';
@UseInBlockPackageVars::l.push(5);
is-deeply @UseInBlockPackageVars::l, [5], 'a push autovivifies a fresh array';

is show(), '1 a 1 1', "the module's own variables are untouched";

{
    use UseInBlockPackageVars;
    is $UseInBlockPackageVars::n, 1, 'a later block that uses the module sees its variable again';
    is show(), '1 a 1 1', "... and the module still sees its own";
}

# vim: expandtab shiftwidth=4
