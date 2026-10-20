unit module Pk::Mod;
use Pk::Mod::Attribute;

# `Attribute` here is the core type: GLOBAL is searched before the package's
# own stash, so the loaded `Pk::Mod::Attribute` role does not hide it.
multi trait_mod:<is>(Attribute:D $attr, :$tagged!) is export {
    $attr does Pk::Mod::Attribute;
}
