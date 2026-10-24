use ReexportsNativeCallTags;
use NativeCall;
unit module ReexportAndUseNativeCall;

our sub pointer-name is export { Pointer.new(-1).^name }
our sub size-name is export { size_t.^name }
