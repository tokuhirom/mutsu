use v6.d;
use experimental :cached;
unit module EvalCachedTraitFixture;

sub cached-probe() is cached is export {
    42
}
