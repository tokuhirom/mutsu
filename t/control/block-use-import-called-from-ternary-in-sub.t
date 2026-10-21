use Test;
use lib 't/lib';

# A routine declared in a block that `use`s a module reaches the module's
# routines through the block's import set. The typed IR (TRIR) used to compile
# such a routine when its body was a ternary or an assignment, and linked the
# imported call by name, where it is unknown (#14140).
plan 4;

{
    use ImportedPredicate;
    sub choose($s) { has-interp($s) ?? 'interp' !! 'plain' }
    is choose('a#{b}'), 'interp', 'an imported call as a ternary condition';
    is choose('plain'), 'plain', 'the other branch';

    sub assign($s) { my $r = has-interp($s); $r }
    is assign('a#{b}'), True, 'an imported call on the right of a declaration';

    sub direct($s) { has-interp($s) }
    is direct('plain'), False, 'an imported call as the whole body';
}
