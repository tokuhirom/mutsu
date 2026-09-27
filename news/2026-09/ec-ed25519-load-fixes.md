# EC's elliptic-curve modules load: six fixes found through a roulette draw

The ecosystem roulette drew **EC 0.6.6** (Lucien Grondin's elliptic-curve cryptography, the
`ed25519` and `secp256k1` modules), whose ledger record read `blocked_load`:
`use ed25519` died with "An exception occurred while evaluating a CHECK". Both modules now
load. Working the suite past that exposed six general interpreter gaps, fixed here. Six more
findings were filed as issues.

## Fixed

- **`callsame` from a user `prefix:<op>` reaches the core operator.** Core infix operators
  were already the implicit final candidate after a user `multi infix:<op>`
  (`native_infix_next_candidate`). The prefix side had no counterpart, so FiniteFields'
  `multi prefix:<->(UInt $n) { callsame() mod $*modulus }` computed `Nil mod p`, i.e. 0. The
  core single-argument prefix operators now live in one helper, `Interpreter::core_prefix_op`
  (`src/runtime/builtins_operators_prefix.rs`). The call fallback and the new
  `native_prefix_next_candidate` both use it.
- **A block-scoped `use` inside a `unit module` no longer leaks its operators.** Inside
  `unit module M` an imported operator is aliased as `M::infix:<->`. `pop_import_scope`'s
  keep-rule treats every package-qualified, non-`GLOBAL::` key as the module's own
  definition, so the alias survived the block. After ed25519's
  `constant d = { use FiniteField; ... }()`, every later `* - 1` in the module ran the modular
  minus. The keys an import scope aliased are now exactly the growth of
  `imported_routine_aliases` since the push, and those are dropped with it.
- **Blob/Buf ~~ Numeric compares the element count**, like an Array. ed25519 dispatches
  `Key.new` on `blob8 $seed where b div 8`. The rule now lives in one helper,
  `positional_numeric_smart_match`, shared by the VM's `pure_smart_match` and the
  interpreter's `smart_match`. The interpreter side lacked even the Array case, which is why
  `3.ACCEPTS([1,2,3])` was False.
- **`Blob.ACCEPTS(Blob)`** compares contents instead of dying with X::Method::NotFound.
- **`$buf[*-1] = ...`** resolves the WhateverCode against the buffer's element count. It used
  to see length 0 and die with "Index out of range", which broke ed25519's scalar clamping
  `$s[*-1] +&= 0b0111_1111`.
- **Whitespace around a capture alias's `=` in a `rule` is not significant.** Sigspace turned
  it into a `<.ws>`, so `rule { k \= $<k> = <.digit>+ }` bound the alias to whitespace and
  never matched. That is the grammar at the top of EC's `t/secp256k1.t`.

## Filed

- #9962: a sigil-less `constant b` shares the scalar key space with `$b`. ed25519's
  `multi method new(blob8 $b where $b == b div 8)` reads the parameter as `b`. This is the
  constants half of what #7959 did for enum keys.
- #9963: a constant imported by a block-scoped `use` does not shadow a same-named type.
  `t/secp256k1.t`'s `grammar G` wins over secp256k1's exported generator `G`.
- #9964: `{ $^x + $x }` run by `map`/`grep`/`...` reads the caller's `$x`. This breaks
  Digest::SHA2's `infix:<√>` tables whenever the caller has a `my $x`.
- #9965: a `blob8`-typed pointy parameter's constraint leaks into the caller's `$h`, a case of
  #8614 that survived.
- #9966: a mainline `CHECK` runs before the script's earlier constants are initialized.
- #9967 (`todo:perf`): secp256k1 point doubling is about 100x slower than rakudo.

Pinned by `t/routines/callsame-prefix-core-candidate.t`,
`t/modules/block-import-operator-unit-module-scope.t` (fixtures
`t/lib/BlockImportModOps.rakumod` and `t/lib/BlockImportModUser.rakumod`),
`t/types/buf-numeric-smartmatch.t`, `t/types/buf-from-end-subscript-assign.t` and
`t/grammar/rule-alias-whitespace.t`.
