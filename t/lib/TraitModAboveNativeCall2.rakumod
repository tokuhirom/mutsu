unit module TraitModAboveNativeCall2;

# A middle layer that only loads the layer which loads NativeCall.
use TraitModAboveNativeCall1;

sub tmanc2-alive() is export { tmanc1-alive() }
