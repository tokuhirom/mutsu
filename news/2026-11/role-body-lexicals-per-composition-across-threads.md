# A role mixed into two types on different threads keeps one body lexical per type

A role body runs once per type that composes it, so `role M { my $n = 0;
method bump { ++$n } }` gives `Foo+{M}` and `Bar+{M}` a counter each. When
the mixins were made on worker threads, mutsu found the role body's
lexicals through a stopgap table keyed by the role's name
(`PublishedMixins::role_lexical`) in which the first composition won, so
every later composition shared the first one's `$n` (`1 2 3 4 5` where raku
prints `1 2 1 2 3`).

Since ADR-17216 slice 4a made the package-lexical table one world table, a
method of a mixin composed on another thread finds the lexicals where the
composing thread filed them. The stopgap is deleted, and each composition
keeps its own lexicals on every thread.
