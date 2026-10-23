use Test;
use lib $*PROGRAM.parent(3).add('lib-default-own-package');

# From the Crypt::SodiumPasswordHash distribution: `sub sodium-hash(Str $p,
# HashAlgorithm $a = HashAlgorithm(crypto_pwhash_alg_default()))` was called
# after `require M <&sodium-hash ARGON2I13>`, so the enum type itself was not
# imported and the default's `HashAlgorithm(...)` died with "Unknown function".
plan 3;

require DefPkg <&describe Fast>;

is describe('a'), 'a Slow', 'a default naming a type the importer lacks resolves in the routine package';
is describe('b', Fast), 'b Fast', 'an explicit argument still binds';
is describe('c'), 'c Slow', 'and the default keeps working on later calls';
