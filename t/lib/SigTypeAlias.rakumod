# Re-exports SigTypeAlias::Types' types as constants, the way upstream
# NativeCall.rakumod exports `size_t` (`my constant size_t is export = ...`).
unit module SigTypeAlias;
use SigTypeAlias::Types;
my constant size_t is export = SigTypeAlias::Types::size_t;
my constant Thing is export = SigTypeAlias::Types::Thing;

# Reads a routine's types inside this module, where the importer's aliases
# are not in scope (upstream's check_routine_sanity does this).
our sub describe(Routine $r) is export {
    ($r.signature.params.map({ .type.^name ~ '/' ~ .type.REPR }),
     $r.returns.^name ~ '/' ~ $r.returns.REPR).flat.join(' ')
}
