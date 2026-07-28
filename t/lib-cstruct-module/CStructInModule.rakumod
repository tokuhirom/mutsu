unit module CStructInModule;
use NativeCall;

# Declared inside a module, so its registered name is package-qualified while
# `nativecast`'s target names it by the short one — the mismatch this file's
# test pins. `MoarVM::Guts::REPRs`' `MVMArrayB` has exactly this shape.
my class Body is repr('CStruct') {
    has uint64 $.elems;
    has uint64 $.start;

    method describe(::?CLASS:D:) { "elems=$!elems start=$!start" }
}

our sub body-class-name() is export { Body.^name }
our sub make-body($p) is export { nativecast(Body, $p) }
