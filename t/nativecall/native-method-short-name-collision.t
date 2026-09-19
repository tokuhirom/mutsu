use Test;
use NativeCall;

plan 1;

# A native method on a CStruct may be registered under its short class name for
# nativecast handles. That descriptor must not shadow an ordinary accessor on a
# different, registered class with the same basename.
class NativeCollision::Thing is repr('CStruct') {
    has int32 $.value;
    method count(--> int32) is native('c') is symbol('getpid') { * }
}

class NativeCollision::Other::Thing {
    has $.count = 42;
}

is NativeCollision::Other::Thing.new.count, 42,
    'registered class accessor wins over another class basename native method';

done-testing;
