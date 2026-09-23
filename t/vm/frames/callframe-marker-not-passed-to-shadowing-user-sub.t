use Test;

# #9093: the parser attaches an internal `__callframe_line` named pair to
# every literal `caller(...)`/`callframe(...)` call, since it cannot know at
# parse time whether the name resolves to the builtin or to a user-declared
# sub of the same name. A user `sub caller`/`sub callframe` shadowing the
# builtin must never see that marker as a genuine named argument.
#
# `sub caller`/`sub callframe` are declared inside a block here (rather than
# at file scope) so this file does not also need to prove the real builtins
# still work when unshadowed -- Raku hoists a sub declaration across its
# WHOLE enclosing lexical scope, so a file-scoped `sub callframe($x)` would
# shadow the builtin for the entire file, including code written before it
# textually. `t/vm/frames/callframe-file-line-same-frame.t` and friends
# already pin the unshadowed builtin.

plan 4;

{
    sub caller($x) { "c$x" }
    is caller(1), 'c1', 'a user sub named caller does not receive the internal marker';

    my @seen;
    for ^3 -> $i { @seen.push: caller($i) }
    is @seen.join(','), 'c0,c1,c2',
        'the marker is filtered on every call, not only the first (light/fast call paths)';
}

{
    sub callframe($x) { "f$x" }
    is callframe(5), 'f5', 'a user sub named callframe does not receive the internal marker';

    my @seen2;
    for ^3 -> $i { @seen2.push: callframe($i) }
    is @seen2.join(','), 'f0,f1,f2', 'same, repeated calls';
}
