# Subscript value adverbs skip the call ladder, and method candidate rows are shared

`ML::TriesWithFrequencies` `trie-create` (#15235) is flat in a callgrind profile, so this slice removes three
unrelated per-call costs that a Raku-line sampling profile (`--profile`) and an allocation count pointed at:

* `%h<a>:p` (and `:k`/`:v`/`:kv`) lowers to the compiler-internal builtin `__mutsu_subscript_adverb`. The call
  went through the whole name-resolution ladder (compiled routine, multi/proto caches, user-sub shadow probes,
  lexical `&` lookup with a `format!`) before ending at the interpreter terminal, which is the only place such a
  name can end. It now enters that terminal directly.
* `is_builtin_function` and the Cool-routine forwarding table were linear scans over ~150 names, asked on every
  builtin call. They are one hash probe now.
* `Registry::user_method_overloads` deep-copied every `MethodDef` of the row on each multi-method call and on each
  `new` of a class declaring a user `new`. The row is a shared `MethodOverloads` (copy on write in the registry
  mutators), and the two hot resolvers borrow it and clone only the candidates they keep.

Measured with callgrind on release builds, warm second run (instructions per operation):

| operation | before | after |
| --- | ---: | ---: |
| `%h<a>:p` | 24.1k | 16.1k |
| `Trie.new(key => .., value => ..)` on a class composing a role with two `multi new` | 45.4k | 42.1k |
| `$o.merge($o, $o2, :merge-clones)` (proto + typed multi) | 84.2k | 83.5k |
| `trie-create` of 400 words (whole run, third warm run) | 1,423M | 1,409M |

The whole-run gain is about 1%: the profile has no single dominant cost, which is itself the finding recorded on
#15235.
