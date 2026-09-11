use v6;
use lib 't/lib';
use Test;

# `sub EXPORT` is per-compunit: each file may declare one, and two files'
# hooks never see each other. mutsu registers the hook under the single
# `GLOBAL::EXPORT` key (a module body runs under GLOBAL), so two modules in one
# load chain that each declared `sub EXPORT` used to compete for it: sub
# declarations are hoisted, so entering the outer module's body registered
# `GLOBAL::EXPORT` before its `use` of the inner one ran, and the inner
# module's own hoisted declaration then died with
# "Redeclaration of routine 'EXPORT'" (#7947).
#
# That is the dominant lizmat idiom -- a small distribution that re-exports a
# dependency under a different name, where both ends compute their exports in
# `sub EXPORT` -- and it blocked ten distributions from loading at all.

plan 6;

{
    use ChainExportOuter;
    is chain-outer(), 'inner',
        'a module with its own sub EXPORT can use another module with one';
}

{
    # The inner module's own hook still works when imported directly, i.e.
    # hiding the enclosing compunit's hook did not consume it.
    use ChainExportInner;
    is chain-inner(), 'inner', "the inner module's own sub EXPORT still runs";
}

{
    use ChainExportMyOuter;
    is chain-my-outer(), 'inner',
        'the two declarations need not share a scope kind (`my sub EXPORT`)';
}

{
    use ChainExportTop;
    is chain-top(), 'inner', 'three deep, with a sub EXPORT at every level';
}

{
    use ChainExportArgs <renamed>;
    is renamed(), 'renamed:inner',
        'the outer hook still receives the end user\'s use arguments';
}

{
    # Side by side has always worked (the first hook is consumed before the
    # second module loads); pin it so the nested fix cannot regress it.
    use ChainExportInner;
    use ChainExportOuter;
    is chain-inner() ~ chain-outer(), 'innerinner',
        'two sub EXPORT modules used side by side still both export';
}
