# Object construction stops cloning a file path per attribute

PR #10060 taught an attribute's auto-generated accessor to report where its
`has` was declared (`Code.line` / `Code.file`), storing the file on
`ClassAttributeDef` as an owned `Option<String>`. The bless path clones the
class's attribute list (`collect_class_attributes`, twice per construction),
so every attribute of every constructed object paid two heap allocations to
carry a path nothing reads during construction. `benchmarks/bench-ctor.raku`
(a 22-attribute class) stepped up by +44 allocations per `Dist.new`, a +16.6%
jump in its `bench-det` allocation series (#10090).

The issue attributed the step to #10058's term namespace by diff shape; a
callgrind run on the benchmark showed `term_binding` is not on the path at
all, and that the `String` clones came from `ClassAttributeDef::clone`
under `collect_class_attributes`. The field is an interned `Symbol` now,
which is `Copy`.

`scripts/bench-det.sh benchmarks/bench-ctor.raku` allocations: 1,564,651 → 1,342,551
(pre-#10060 baseline 1,342,212); instructions ~1,538M → 1,467.7M
(baseline 1,463.1M). `bench-ctor+jit`: 1,345,372 allocations, 1,462.3M
instructions.

`tests/attribute_source_file_alloc_budget.rs` pins it with a counting
allocator: the per-attribute, per-construction allocation slope of a
`bless`/`TWEAK` class must stay below the value it had with the `String` (2.15 before, 1.15 after).
