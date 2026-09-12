use v6;
unit module ConstantTypeAliasExport;

class Shape is export { method sides { 3 } }

# A `constant` bound to a bare type name is a TYPE ALIAS: usable wherever a
# type name is. Both spellings appear in the wild -- `Gnome::N` writes the
# sigilless one (`constant \GType is export = uint64`).
constant AliasedInt is export = Int;
constant \AliasedNative is export = uint64;
constant AliasedShape is export = Shape;
