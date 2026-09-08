unit module EvalImport::Inner;
use NativeCall;

# `nativecast` is imported into THIS module's scope by the `use NativeCall`
# above. The point of the test is that it stays resolvable from here even when
# this module's first load happened inside an EVAL.
sub inner-probe() is export { defined(&nativecast) ?? 'visible' !! 'MISSING' }
sub inner-value() is export { 42 }
