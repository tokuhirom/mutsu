# A rule's object argument is the object inside its code blocks

A `token`/`rule`/`regex` parameter bound to an object now holds *that* object
inside the rule's `{ … }` blocks and `<?{ … }>` assertions. Before, mutsu turned
a parameterized rule into pattern text by baking each bound value into the code
blocks as its `.raku`. That works for numbers and strings, but not for an object:
the re-parsed text was at best a fresh object without the caller's state. When
the caller's variable had been captured by a closure, it was read as a shared
cell, whose `.raku` re-parsed into the coercion type `Res(Any)`. So
`<?{ $resources.known-name(...) }>` died with "No such method" on a type object.

A value whose literal form cannot round-trip is now left unbaked: closures,
instances, mixins, proxies, promises and channels, and any container holding
one (looking through scalar and cell containers). The matcher binds the real
value in its env for the subrule's resolve-and-match window, reusing the
install/restore that `$*` parameters and block-valued named arguments already
used. Positional closure arguments (`<e(-> { … })>` into `regex e($f)`) work
through the same path; before, `$f` was `Nil`.

Found with DSL::Shared 0.2.11, where
`t/Entity-names-parsing-via-resources-access-object.rakutest` goes from dying
before its first test to 4/4.
