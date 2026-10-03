# Fixture for t/routines/signature/signature-type-alias-constant.t: the shape
# upstream NativeCall::Types declares its C integer types in.
unit module SigTypeAlias::Types;
our native size_t is Int is ctype<size_t> is unsigned is repr<P6int> { }
our class Thing { }
