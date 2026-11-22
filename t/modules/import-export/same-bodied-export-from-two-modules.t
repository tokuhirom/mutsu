use lib $?FILE.IO.parent(3).add('lib');
use Test;

# Two modules that each export a same-named sub with the same body are two
# routines: each block's import is its own module's (#18742).

plan 4;

{
    use SameBodyExportA;
    like &same-body-who.file, /SameBodyExportA/, 'the first block sees A\'s routine';
    is same-body-who().^ver, '6.c', '... which runs as 6.c code';
}
{
    use SameBodyExportB;
    like &same-body-who.file, /SameBodyExportB/, 'the second block sees B\'s routine';
    is same-body-who().^ver, '6.e', '... which runs as 6.e code';
}
