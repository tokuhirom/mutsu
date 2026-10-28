# Method calls share their signature, and a `{*}`-only proto no longer runs as a method

`ML::TriesWithFrequencies` `trie-create` (#15235) is pure method-call code, and a callgrind profile of it
showed the method-call machinery doing work per call that depends only on the declaration:

* `MethodDef` held its parameter names and every `ParamDef` by value, so each resolved-method cache hit
  deep-cloned them. Both are `Arc`-shared now.
* `Registry::method_entry_proto` cloned the proto's whole `FunctionDef` (parameters and body) on every call
  of a method that has a `proto`. The entry is an `Arc<FunctionDef>`.
* `call_compiled_method_in_unit` cloned each parameter name and `ParamDef` into a fresh `Vec` on every
  call just to drop a leading invocant parameter; without one (the common case) it borrows the
  declared slices.
* A `proto method m(|) {*}` built a `MethodDef`, ran bytecode and bound a signature only to reach the `{*}`
  that redispatches. For a `{*}`-only proto without scalar `is rw`/`is raw` parameters the dispatch frame is
  now pushed and the winning candidate is run directly, as rakudo's onlystar dispatch does.
* A typed or defaulted named parameter (`Num :$value = 1e0`) sent the whole call to the general binder, a
  fixed ~28k instructions. `MethodDef` now caches a per-parameter plan (its type check and verified
  constant default), so the compiled-method fast path binds such calls itself.
* A read of an `@`/`%` accessor collected every attribute definition of the MRO to find one declaration.

Measured with callgrind on the release binary (instructions per call, second run):

| call | before | after |
| --- | ---: | ---: |
| `method m(@c)` | 21k | 21k |
| `method m(@c, Num :$value = 1e0)` | 49k | 22k |
| `method m(@c, Num :$value = 1e0, Num :$bottomValue = 1e0, Bool :$verify-input = True)` | 58k | 25k |
| `proto method pm(|) {*}` + one `multi` candidate | 135k | 90k |

`trie-create` of 6400 word pairs went from about 7.2 s to about 5.5 s on a 4-core container (rakudo
0.74 s on the same box), so #15235 stays open: the remaining cost is spread over type-name resolution,
`Symbol` interning and the general binder's per-parameter environment bookkeeping.
