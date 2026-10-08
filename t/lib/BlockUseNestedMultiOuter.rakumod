unit module BlockUseNestedMultiOuter;
use BlockUseNestedMultiExp;
sub outer-multi-probe() is export { nested-mexp(1) ~ nested-mexp('a') }
