use Test;
use lib 't/fixtures/export-hook-colonpair/lib';
use ExportHookColonpairUser;

# A named argument spelled `:name(v)` inside a `sub EXPORT` hook (here in a
# routine the hook declares) is not an exported name: it must not shadow a
# same-named parameter of the module's own routines when they run from a
# method.
plan 2;

is via-sub(1), 'pretty', 'called from a sub';
is ExportHookColonpairUser.new.via-method(1), 'pretty', 'called from a method';
