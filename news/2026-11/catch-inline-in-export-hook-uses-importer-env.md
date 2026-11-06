# A CATCH reached through a dying `sub EXPORT` hook runs in the importer's env

When a module's `sub EXPORT` rejects a `use` argument, the importer's inline
`CATCH` (ADR-0072) runs at the throw site, inside the hook. The hook's call
frame saved the module's env, so the handler's captured variables read `Nil`
and its writes were lost (BinaryHeap `Test::throws-like "use BinaryHeap <Foo>",
..., message => ...`). The use site now registers the importer's env for the
hook's frame depth; the handler runs against it, writes land in it, and it is
what the use restores afterwards. Completes #16968.
