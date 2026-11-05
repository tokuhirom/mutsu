# A stash read through a missing package is an empty stash

`Foo::Bar::<$x>`, `Foo::Bar::.keys` and `A::B::<$x>:exists` with no such package died with
`Could not find symbol '&Bar' in 'GLOBAL::Foo'` (#9845). That is the legacy frontend's behavior;
rakudo's RakuAST frontend, the oracle, answers `Any` / an empty stash / `False`. The check
(`runtime/accessors_stash_exists.rs`) is removed and `t/modules/package-stash-missing-package.t`
now pins the RakuAST answers. Found by the raku-doc differential (`Language/packages.rakudoc:80`).
