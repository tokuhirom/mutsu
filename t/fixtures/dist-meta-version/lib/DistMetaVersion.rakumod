unit module DistMetaVersion;

# A META6.json that spells only `version` and `author`: `$?DISTRIBUTION.meta`
# still answers `ver`, `auth` and `api`, as Rakudo's distribution does. The
# attribute default mirrors HTTP::Tiny's user agent string.
sub dist-meta() is export { $?DISTRIBUTION.meta }

class Agent is export {
    has Str $.agent = self.^name ~ '/' ~ $?DISTRIBUTION.meta<ver> ~ ' Raku';
}
