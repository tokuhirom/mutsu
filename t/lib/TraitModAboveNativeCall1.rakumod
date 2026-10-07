unit module TraitModAboveNativeCall1;

# The innermost layer: the only module that itself loads NativeCall.
use NativeCall;

sub tmanc1-getpid() returns int32 is native is symbol('getpid') { * }

sub tmanc1-alive() is export { tmanc1-getpid() > 0 }
