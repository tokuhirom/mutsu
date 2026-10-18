use Test;

# #13877: a module first loaded by a `require` inside a bare block kept its
# routines when the block exited, but lost its `our proto sub`s: the block's
# routine-registry restore reset the proto tables wholesale. A later
# `require` of the loaded module is a no-op, so `::('M::&f')` was Nil in
# every later scope. Checked against rakudo.
plan 6;

use lib 't/lib';

sub lookup() {
    require ::('ProtoSurvivesBlock');
    ::('ProtoSurvivesBlock::&psb-it')
}

{
    my &it = lookup();
    is it('a'), 'it:a', 'the proto dispatches in the block that loaded the module';
}
{
    my &it = lookup();
    ok &it.defined, 'a later block finds the proto again';
    is it('b'), 'it:b', 'and it dispatches to the Str candidate';
    is it(3), 'int:3', 'and to the Int candidate';
}
{
    require ::('ProtoSurvivesBlock');
    is ::('ProtoSurvivesBlock::&psb-plain')(), 'plain', 'a plain routine is still there';
}
is lookup()('c'), 'it:c', 'so is the proto outside any block';
