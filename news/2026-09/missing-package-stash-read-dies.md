# A stash read through a non-existent package now dies as in rakudo

`Foo::Bar::<$quux>` and `Foo::Bar::.keys` used to answer `Nil` and `()` when no
`Foo::Bar` package existed at all: mutsu builds a package stash from flat
qualified keys, and a spelling with no keys behind it simply produced an empty
stash. Rakudo resolves the package first and dies with
`Could not find symbol '&Bar' in 'GLOBAL::Foo'`
([#9845](https://github.com/tokuhirom/mutsu/issues/9845)).

Both the whole-stash read (`GetPseudoStash`) and the fused one-key read
(`GetPseudoStashKeyed`, also used by a keyed stash assignment) now check that a
qualified package exists before reading it. As in rakudo, the prefix is
reported under `GLOBAL::` exactly when its first component is unknown
(`A::B::C::<$x>` with a class `A` says `in 'A::B'`). An explicit `GLOBAL::`
qualifier, pseudo-packages, and names rooted in a built-in type are left alone.

A package counts as existing when it is declared (class, role, module, package,
enum, stub), exported, bound in the env, a built-in type, or when some live
qualified symbol sits under it — the implicit namespace `our $Foo::Bar::x`
creates. That last probe must not scan the env, so the qualified-name index of
#9171 grew a companion: every interned qualified name is also recorded under
each package spelling it names a member of, and the check probes only those
names (`O(k)`). The keyed read pays it only on a miss; a hit already proves the
package exists.
