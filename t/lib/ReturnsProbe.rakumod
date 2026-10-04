unit module ReturnsProbe;

# Reads a routine's return type from this module's scope, where the caller's
# lexical types are not visible -- as upstream NativeCall's `$routine.returns`
# and `$signature.returns` do.
sub returns-probe(&r) is export {
    (&r.returns.hi, &r.signature.returns.hi).join(',')
}
