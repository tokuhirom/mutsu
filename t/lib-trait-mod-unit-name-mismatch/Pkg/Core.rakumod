# The file is loaded as `Pkg::Core` but declares `unit module Core`.
unit module Core;

multi sub trait_mod:<is> (Attribute $attr, :$aliased-by!) is export {
    $attr.package.^add_method: $aliased-by, anon method { $attr.get_value(self) }
}
