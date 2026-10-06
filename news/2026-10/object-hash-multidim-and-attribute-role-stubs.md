# Object-hash multi-dim subscripts, Attribute role stubs, `.loaded` for `use`d modules

Working the `Injector` distribution (the interpreter side of its suite) fixed four general gaps:

- A role stub `method package {...}` mixed into an `Attribute` (or `var`/`name` into a `Variable`)
  no longer fails with "must be implemented"; stubs never shadow the native method either.
- `my %h{Str:D; Str:D; Str:D}` keys its object hash by the first dimension only, and multi-dim
  reads, writes, `:exists` and call arguments now agree on the `.WHICH` key. Previously a second
  write under an existing first key was silently lost.
- `CompUnit::Repository::FileSystem.loaded` lists the modules `use` loaded from that repository.

The remaining `Injector` gap is `Variable.block.add_phaser` (#12131).
