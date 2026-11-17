# Hands HookFamilyBase's exports out from a `sub EXPORT` Map, as Red's
# `use Red:api<2>` does with `Red::Traits::EXPORT::ALL::`.
use HookFamilyBase;
sub EXPORT(|) { Map.new: HookFamilyBase::EXPORT::ALL:: }
