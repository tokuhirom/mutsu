use v6;
use Test;

# A routine of a module block reaches an `our sub` of a `my package` the block
# declared, through the package's short name, after the block has ended.
# mutsu found it only while the block ran (#18520).

plan 4;

module MyPkgRoutine {
    my package P { our sub f { 42 } }
    our sub inner { P::f() }
    our sub now { P::f() + 1 }
    our $during = now();
}
is MyPkgRoutine::inner(), 42, 'a block routine called after the block';
is $MyPkgRoutine::during, 43, 'the same call while the block runs';

class P { }
module MyPkgShadow {
    my package P { our sub f { 'mine' } }
    our sub inner { P::f() }
}
is MyPkgShadow::inner(), 'mine', 'the my package wins over an outer type of the same name';

throws-like { EVAL 'MyPkgRoutine::P::f()' }, Exception, 'the my package stays private to the block';
