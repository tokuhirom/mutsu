# The default constructor reads a `.map` Seq a declining `new` was handed

A class with its own `multi method new` whose candidates do not take a call's named arguments
falls back to the default constructor (`Mu.new(*%attrinit)`). When such a call passed a
`.map`/`.grep`/gather Seq for an `@` or `%` attribute, the attribute came out empty:

```raku
class C { has @.a; multi method new(Int $x) { self.bless } }
say C.new(a => (1, 2).map({ $_ })).a;   # was [], now [1 2]
```

The VM reads such a Seq at the call boundary only for a built-in method. A class's own `new`
binds the Seq itself, so the boundary left it unread, and the default constructor that ran
after the user `new` declined coerced the unread Seq to an empty Array. The fallback (both the
native construction lane and the interpreter's default constructor) now reads the named
arguments the way a built-in method's boundary does.

This fixes ML::TriesWithFrequencies `t/08-overall-from-JavaTrie.rakutest`
(`JavaTrieCloneEquality1`): `Trie.clone` builds `Trie.new(children => self.children.map(...))`,
and the role declares only positional `new` candidates, so every clone lost its children.
