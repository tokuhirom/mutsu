use Test;
use lib 't/lib';

# An imported proto family (a compunit-scoped multi, as Array::Sorted::Util's
# `finds`) remembers its winner per argument types and its visibility per
# unit (#15786); calls with alternating argument types must keep resolving
# exactly as a fresh dispatch would.

plan 6;

use ImportedProtoFamilyMemo;

my @r = (^4).map: -> $i { $i %% 2 ?? locate([5, 6, 7], 6) !! locate("abc", "c") };
is @r.join(","), "1,2,1,2", 'alternating Array / Str candidates';
ok found(locate([1, 2], 2)), 'found';
nok found(locate([1, 2], 9)), 'not found answers the module-private class';

{
    my proto sub locate(|) {*}
    my multi sub locate($x, $y) { "local" }
    is locate([1], 1), "local", 'a local family shadows the imported one';
}
is locate([1, 2, 3], 3), 2, 'the imported family again after the block';
is EVAL('locate("xyz", "z")'), 2, 'from an EVAL unit';
