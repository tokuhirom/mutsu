use Test;
use MONKEY-SEE-NO-EVAL;

# #17216: an `our sub` or a top-level `package` a worker thread declares
# belongs to the program's GLOBAL, as on rakudo, so the main thread reaches it
# through every stash spelling once the worker is done.

plan 10;

await start { EVAL 'our sub from-worker-ows { "ows" }' };
is EVAL('GLOBAL::<&from-worker-ows>()'), 'ows', 'GLOBAL::<&f>';
is EVAL('&GLOBAL::from-worker-ows()'), 'ows', '&GLOBAL::f';
is &::("from-worker-ows")(), 'ows', 'indirect &::("f")';
is OUR::<&from-worker-ows>(), 'ows', 'OUR::<&f>';

await start { EVAL 'package Q17216 { our sub qf { "qf" } }' };
is EVAL('Q17216::qf()'), 'qf', 'Pkg::f()';
is EVAL('Q17216::<&qf>()'), 'qf', 'Pkg::<&f>';
is EVAL('Q17216::.keys.sort.join(",")'), '&qf', 'Pkg:: stash keys';
is EVAL('Q17216.WHO.keys.sort.join(",")'), '&qf', 'Pkg.WHO keys';
is EVAL('Q17216.^name'), 'Q17216', 'the bare package name is its type object';

# A name no thread declared is still undeclared.
throws-like { EVAL 'NopeUndeclared.^name' }, X::Undeclared::Symbols,
    'an undeclared name still dies';
