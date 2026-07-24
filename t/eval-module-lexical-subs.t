use v6;
use lib 't/lib';
use Test;

# A module loaded inside a scope that restores the routine registry (a bare
# block, or `EVAL`) must keep its OWN routines afterwards: `loaded_modules` is
# never rolled back, so re-`use`ing it is a no-op that could not bring them
# back. Dropping them left the module half-loaded -- an export the scope handed
# out still ran, but its calls to the module's file-scoped helpers died with
# "Unknown function".
#
# This is how File::Temp's upstream t/03-tempfile loads the module (it installs
# its own END phaser before File::Temp's), which failed with
# "Unknown function: make-temp".

plan 5;

# --- EVAL: the module's file-scoped helper survives the EVAL's registry restore
my (&doubled, &tripled) := 'use EvalModHelper; &doubled, &tripled'.EVAL;
is doubled(5), 10, 'an exported sub from an EVAL-loaded module reaches its file-scoped helper';
is tripled(5), 15, 'an `our` export from an EVAL-loaded module reaches it too';
is doubled(7), 14, 'and keeps working on a later call';

# --- bare block: same rule, a block is also a registry-restoring scope
my $from-block;
{
    use EvalModHelper;
    is doubled(3), 6, 'the import is visible inside the importing block';
    $from-block = &doubled;
}
is $from-block(6), 12, 'a sub captured from a block-scoped `use` still reaches its helper';
