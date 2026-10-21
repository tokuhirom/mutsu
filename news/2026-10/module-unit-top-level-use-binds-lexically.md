# A module unit's top-level `use` binds its routines lexically, and the `top_level` carve-out is gone

ADR-8751 stage 1: a module file's mainline now declares the import sets of its own top-level `use`s (`Compiler::defer_unit_lexical_use_sets`, as the main unit does), so a routine, multi family or operator a module imports is bound in the module's lexical set instead of aliased into the unit-gated registry. Variables, constants and types a module imports keep their env aliases (the module's import scope removes them), as an `EVAL` unit's sets do.

Two defects surfaced while moving the Cro::HTTP battery over and are fixed here:

- A closure body recompiled from its AST (a `supply` body run through `call_sub_value`) took its import chain only from its own call sites. A `supply { whenever IO::Socket::Async.listen(...) -> $s { imported-sub($s) } }` has none in the outer body, so the nested `whenever` closure lost the module's sets and the call fell through to the registry (`Unknown function: nodelay` in `Cro::TCP`). `CompiledCode::enclosing_import_chain` records the sets around each closure where it is attached, and the recompile starts inside them.
- `--dump-bytecode` declares the unit's use sets the way a run does (module mode for `.rakumod`/`.pm6`/`.pm` files).

With module units bound lexically the `LexicalImportCapture::top_level` carve-out, which kept a top-level `use`'s multi families on the registry path, is removed.
