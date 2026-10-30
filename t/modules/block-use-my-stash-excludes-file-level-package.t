use Test;
use lib 't/lib';

# A nested block's `MY::` lists what the block itself imported, not the
# package of a file-level `use` (#15532).
plan 3;

{
    use BlockUseNestedInner;
    my @names = MY::.keys.grep(/<[A..Za..z]>/).sort;
    is-deeply @names.List, ('&inner-probe', 'BlockUseNestedInner'),
        'the block lists its own import only';
    nok MY::<Test>:exists, 'the file-level use Test is not in the block pad';
}

ok MY::<Test>:exists, 'the file-level package is still the unit\'s';
