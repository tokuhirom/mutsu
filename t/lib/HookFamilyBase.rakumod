unit module HookFamilyBase;
multi sub hff(Int $x) is export { "int" }
multi trait_mod:<is>(Attribute $attr, :$hff-index! (&expression, *%options) --> Empty) is export {
    $*HFF-SEEN = "index " ~ %options.keys.sort.join(",");
}
