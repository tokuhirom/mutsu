use Test;

# From CSS::TagSet (CSS::Module / CSS::Properties): `&!index()` must call the
# invocant's `&!index`, not a same-named private attribute of a class whose
# method is further up the call chain.
plan 2;

class M {
    has &.index;
    method index { &!index() }
}

class P {
    has $!index;
    has M $.m;
    submethod TWEAK {
        $!index = [9];
        $!result = self!deep;
    }
    has $.result;
    method !deep { helper($!m) }
    sub helper($m) { $m.index }
}

my $p = P.new(:m(M.new(:index({ [1, 2] }))));
is-deeply $p.result, [1, 2], 'callee uses its own &!index';
is-deeply $p.m.index, [1, 2], 'and again after construction';
