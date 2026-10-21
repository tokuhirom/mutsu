unit module ProxyArgMultiFamily;
use ProxyArgMultiCaller;

multi sub kind(%a, %b) { 'hash' }
multi sub kind(List:D $a, List:D $b) { 'list' }
multi sub kind(Mu $a, Mu $b) { 'mu' }

sub run(\a, |c) is export { call-op(a, &kind, |c) }
