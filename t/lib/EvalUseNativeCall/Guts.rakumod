unit module EvalUseNativeCall::Guts;
use NativeCall;

# Calls a NativeCall prelude helper from its OWN body, by bare name.
sub guts-size() is export { nativesizeof(Pointer) }
