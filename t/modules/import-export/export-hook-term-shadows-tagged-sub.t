use Test;
use lib 't/lib';

# #9339: a sigilless term installed by `sub EXPORT` must win over a same-named
# tag-exported sub for a BARE use of the name (`t`, `t.hi`); only `t()` calls
# the sub. The static module scan learns `t` is a routine from
# `sub t is export(:t)`, so the parser used to compile the bare `t` as a call,
# which failed with "Unknown function: t" whenever the tag was not imported.
#
# #9389: a colonpair tag (`use M :t`) still imports the module's `is
# export(:t)` symbols when the module also has a `sub EXPORT` hook -- the
# hook only receives the positional `use` arguments -- in a block, when the
# tagged sub is nested in a class, and across a later re-`use`.

plan 16;

# The hook's module is first loaded by a block, so every later import below
# re-runs its EXPORT (`rerun_module_export`).
{
    use ExportHookTermVsTaggedSub;
    is t.hi, 'hi', 'use M; t.hi reaches the EXPORT-installed term';
    my $x = t;
    isa-ok $x, ExportHookTermVsTaggedSub, 'use M; a bare t in an assignment is the term';
}

# A module's own routine that uses the term it imported through a re-run
# hook, called from this file and from a `start` thread (the thread clone
# used to forget which names a hook installed).
{
    use ExportHookTermUser;
    is ExportHookTermUser.new.go, 'hi', "a module's method sees the term it imported";
    is (await start { ExportHookTermUser.new.go }), 'hi', '... also when run on another thread';
}

{
    use ExportHookTermVsTaggedSub <tt>;
    is tt.hi, 'hi', 'use M <tt>; the hook installs the term under the given name';
}

# A block-scoped tagged import of a hook module installs the tagged sub.
{
    use ExportHookOtherTerm :u;
    is u(), 'from-sub-u', 'use M :u in a block; the tagged sub is imported';
    is u, 'from-sub-u', 'a bare tagged sub is called when the hook installs a different name';
    is other, 42, 'the hook-installed term is visible alongside it';
}

# The tagged sub is declared inside the module's class body.
use ExportHookTermVsTaggedSub :t;
is t(), 'from-sub', 'use M :t; t() calls the tag-exported sub nested in a class';
is t.hi, 'hi', 'use M :t; a bare t still names the term';
is (t).^name, 'ExportHookTermVsTaggedSub', 'use M :t; a parenthesized bare t is the term';

# A later re-`use` in a block does not drop the earlier tagged import.
{
    use ExportHookTermVsTaggedSub;
    is t.hi, 'hi', 'the block re-use installs the term';
}
is t(), 'from-sub', 'a later block re-use keeps the earlier tagged import';

# An unknown tag is still an error, hook or not.
throws-like { EVAL 'use ExportHookOtherTerm :nosuch' }, X::Import::NoSuchTag,
    'an undeclared tag of a hook module is rejected';

# Positional arguments go to the hook, not to the tag import.
{
    use ExportHookTermVsTaggedSub 'ttt';
    is ttt.hi, 'hi', 'a positional use argument names the hook term';
    ok !(try EVAL 't()'.subst('t', 'ttt') ~ ';1'), 'the positional argument imports no tag';
}
