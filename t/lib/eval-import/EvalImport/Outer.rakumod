unit module EvalImport::Outer;
use NativeCall;
use EvalImport::Inner;

sub outer-probe() is export { inner-probe() }
