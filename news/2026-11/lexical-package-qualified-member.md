# A lexical package's qualified members belong to its own declaration

A lexical package (`my package tmp`, `my class Q`) is a separate package for
each declaration, stored under a mangled name (`tmp\0<id>`), and `tmp.WHO`
was already that declaration's own stash. A qualified name such as `tmp::Foo`,
`$Q::v` or `@Q::a` did not follow it, though. Every member was also mirrored
under its facing spelling (`tmp::Foo`, from #16620), so two declarations of
one spelling shared that key: the last binding won everywhere, and a block read
another block's member (#17449, CompUnit::Util's `set-in-WHO multiple`).

The mirror is gone. The head of a qualified name now resolves lexically: when
`tmp` is bound in scope to a lexical package, `tmp::Foo` reads and writes
`tmp\0<id>::Foo`, the member of the declaration in scope. That holds in a
closure that outlives its block, too. The END-phaser pre-install of a lexical
package's `our` symbols uses the same storage key. Before, it put an `Any`
under the facing key, which only the mirror had been covering up.
