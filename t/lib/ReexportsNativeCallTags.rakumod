use NativeCall;
unit module ReexportsNativeCallTags;

# The NativeLibs idiom: bind every tag stash of NativeCall as this module's own.
CHECK for NativeCall::EXPORT::.keys {
    UNIT::EXPORT::{$_} := NativeCall::EXPORT::{$_};
}
