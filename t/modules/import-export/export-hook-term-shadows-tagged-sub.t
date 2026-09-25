use Test;
use lib 't/lib';

# #9339: a sigilless term installed by `sub EXPORT` must win over a same-named
# tag-exported sub for a BARE use of the name (`t`, `t.hi`); only `t()` calls
# the sub. The static module scan learns `t` is a routine from
# `sub t is export(:t)`, so the parser used to compile the bare `t` as a call,
# which failed with "Unknown function: t" whenever the tag was not imported.

plan 5;

# The tagged-import cases (`use M :t`, `use M2 :u`) live at file scope in
# export-hook-term-shadows-tagged-sub-imported.t: a tagged import of an
# EXPORT-hook module inside a block currently loses its tagged subs (#9389).

# A module's own routine that uses the term, called from this file and from a
# `start` thread (the thread clone used to forget which names a hook installed).
# First in the file: when the hook's module was already loaded by an earlier
# scope, the second import's module-scope binding is not found yet (#9389).
{
    use ExportHookTermUser;
    is ExportHookTermUser.new.go, 'hi', "a module's method sees the term it imported";
    is (await start { ExportHookTermUser.new.go }), 'hi', '... also when run on another thread';
}

{
    use ExportHookTermVsTaggedSub;
    is t.hi, 'hi', 'use M; t.hi reaches the EXPORT-installed term';
    my $x = t;
    isa-ok $x, ExportHookTermVsTaggedSub, 'use M; a bare t in an assignment is the term';
}

{
    use ExportHookTermVsTaggedSub <tt>;
    is tt.hi, 'hi', 'use M <tt>; the hook installs the term under the given name';
}
