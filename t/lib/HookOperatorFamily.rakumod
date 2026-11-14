# Hands the importer an operator family through a `sub EXPORT` map, as Red's
# `Red.exports` does with `Red::Operators::EXPORT::ALL::`.
use HookOperatorFamily::Ops;
sub EXPORT() { %( HookOperatorFamily::Ops::EXPORT::ALL:: ) }
