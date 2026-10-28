# A method no proto declares skips the proto lookup, and a declared class is not chased as an alias

Second slice of the `ML::TriesWithFrequencies` `trie-create` profile (#15235):

* `lookup_proto_method` walked the class's MRO for every method call as soon as the program declared any
  `proto method`, interning two names per level. `Registry` keeps the set of method names some proto is
  declared for, so every other call answers in O(1); a hit interns the name once and probes by `Symbol`.
* The `TypeCheck` opcode (`my Foo::Bar $x = ...`) ran `resolve_type_alias_chain` -- a chain of by-name
  env, unit-lexical, package-chain and `our` misses, ~35k instructions per declaration -- before
  accepting a class that is registered under that very spelling. A registered class or role is a type in
  its own right (`type_alias_target` already draws the same line), so the walk is skipped for it.
* A read of an `@`/`%` accessor needed only an attribute's sigil and rw-ness but cloned its whole
  `ClassAttributeDef`; `find_public_class_attribute` now returns a `Copy` shape.
* `cstruct_class_name` answers `None` at once when no `CStruct`/`CUnion` class exists, instead of
  interning and scanning on every accessor read.

`trie-create` under callgrind (300 words, release, second run): 1,079M -> 1,011M instructions. Wall clock
for 6400 words, paired against the pre-#15274 binary on the same box: 7.7-9.2 s -> 4.7 s. #15235 stays
open (goal: <= 3x rakudo).
