use Test;
use lib $?FILE.IO.parent(3).add('lib');

# A LEAVE phaser a module's `sub EXPORT` attaches to the importing block runs in
# that block's dynamic context: a `$*name` it reads is the one the same EXPORT
# exported into the block, not the stale binding of the EXPORT frame the phaser
# closure was created in (FINALIZER's `{ $*FINALIZER.FINALIZE }`).

plan 4;

my @*PHASER-LOG;

{
    use AttachLeavePhaserDynamic;
    is $*HOLDER.tag, 'exported', 'the exported dynamic is visible in the block';
    is @*PHASER-LOG.elems, 0, 'the phaser has not run inside the block';
}
is @*PHASER-LOG.join('|'), 'holder exported',
    'the phaser read the exported dynamic when the block was left';

{
    use AttachLeavePhaserDynamic;
    $*HOLDER.tag = 'changed';
}
is @*PHASER-LOG.join('|'), 'holder exported|holder changed',
    'a second entry reads its own block\'s exported dynamic';
