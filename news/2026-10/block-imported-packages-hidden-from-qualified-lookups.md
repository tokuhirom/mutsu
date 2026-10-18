# A block's `use` no longer grants `::('M::Foo')` after the block or to its callees

A package-qualified indirect lookup (`::('M::Foo')`, `::('&M::f')`) found the
packages a block-scoped `use M` loaded everywhere: after the block, and in a
routine written outside the block that the block called (#13801):

```raku
sub callee { (try ::('ClsM::Foo').hi) // 'no pkg' }
{ use ClsM; say callee() }                        # rakudo: no pkg; mutsu: foo
say (try ::('ClsM::Foo').hi) // 'no pkg after';   # rakudo: no pkg after; mutsu: foo
```

The qualified bareword `ClsM::Foo` and the call `ClsM::f()` already went
through the package-visibility gate (#7797), which a block's `use` satisfies
only while the block runs. The indirect lookup now passes the same gate.
The block's merge of the module is also a lexical binding now: a top-level
routine's frame hides it, as it hides the block's other imports, so a callee
is not granted it either.
