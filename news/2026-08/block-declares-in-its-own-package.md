# A block declares in the package it was written in, whoever calls it

`vm_closure_dispatch` already ran a closure body under the package the closure
was *declared* in, so that nested-class short names and `our` variables resolve
when a foreign frame invokes it. The restore was skipped when that package was
`GLOBAL` — presumably as a "nothing to do" shortcut. It is not: the caller's
package is whatever the *callee* set, so a file-scope block invoked from inside a
module kept the module's package and everything it declared was named after the
module.

```raku
# CallBlk.rakumod:  sub call-it(&blk) is export { blk() }
use CallBlk;
call-it { my class Inner {}; say Inner.^name }
# rakudo: Inner        mutsu (before): CallBlk::Inner
```

`Test::Util`'s `group-of` invokes the block it is handed, so every class declared
inside a `group-of N => 'desc' => { … }` was named `Test::Util::Foo`, and
`roast/integration/error-reporting.t` compared

```
Cannot resolve caller foo(Test::Util::RT129800:U: :foo(Test::Util::Foo)); …
```

against rakudo's `foo(RT129800:U: :foo(Foo))`. That file only started failing
once `news/2026-08/pair-subsignature-dispatch.md` let the *real* `group-of` run
at all — mutsu's native provider had been answering it, and the native one
invokes the block without crossing a module frame.

The condition is now "the closure's package differs from the current one",
`GLOBAL` included. A block declared inside a package still declares in that
package, and a same-package call still skips the save/restore.

Pin: `t/block-declares-in-its-own-package.t` (with
`t/lib/CallerBlockPackage.rakumod`); passes verbatim under `raku`.
