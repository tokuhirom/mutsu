unit module HookFamilyOther;
multi sub hff(Str $x) is export { "str" }
multi trait_mod:<is>(Attribute $attr, :$hff-exclude! (&expression, *%options) --> Empty) is export {
    $*HFF-SEEN = "exclude";
}
