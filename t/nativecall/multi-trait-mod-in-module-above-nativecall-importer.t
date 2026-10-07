use lib 't/lib';
use Test;

# NativeCall's `sub EXPORT` hands the using scope an `&trait_mod:<is>`
# dispatcher. A module several `use` levels above the one that loads
# NativeCall still sees that dispatcher in its scope, and its own
# `multi trait_mod:<is>` candidates join the family -- they are not a second
# plain `sub` of the same name. Loading Cro::HTTP::Router (three levels above
# OpenSSL's NativeCall import) died with "Redeclaration of routine
# 'trait_mod:<is>'". Every expectation was verified against Rakudo.

plan 6;

lives-ok { require ::('TraitModAboveNativeCall3') },
    'a module declaring multi trait_mod:<is> above a NativeCall importer loads';

use TraitModAboveNativeCall3;

ok tmanc3-alive(), 'the layers still reach the native routine through the chain';

sub marked(Int $x is marked) { $x }
is marked(7), 7, 'the module\'s own trait candidate applies';

sub tagged(Int $x is tagged) { $x }
is tagged(8), 8, 'a second candidate of the same module applies too';
is &tagged.signature.params[0].marked-by, 'marked', 'and does its role onto the parameter';

use NativeCall;
sub strlen(Str --> size_t) is native { * }
is strlen('abcd'), 4, 'NativeCall\'s own is native candidate still works beside them';
