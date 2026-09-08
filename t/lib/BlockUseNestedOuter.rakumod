unit module BlockUseNestedOuter;
use NativeCall;
use BlockUseNestedInner;

sub outer-probe() is export { inner-probe() }
