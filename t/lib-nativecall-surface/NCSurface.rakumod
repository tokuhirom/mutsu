unit module NCSurface;
use NativeCall;

# A `unit module` is where prelude scoping breaks: the runtime package switch is
# emitted at the top of the unit, so an unqualified prelude declaration would
# register as `NCSurface::Pointer` and be a different type from the builtin.
# Every type object and helper must still be the global one here.
sub surface-names() is export {
    (bool.^name, ssize_t.^name, void.^name, Pointer.^name)
}

sub opaque-is-pointer() is export { OpaquePointer === Pointer }

sub managed-name() is export { explicitly-manage('mutsu').^name }

sub refreshed() is export { refresh(1) }
