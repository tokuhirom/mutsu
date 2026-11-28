# Built-in type name tables are answered by a hashed probe

Three fixed name tables sat under nearly every type-name question the
interpreter asks, and each answered a miss by comparing the name against many
candidates:

- the built-in class catalogue (ADR-19206), behind every
  `ClassTable::contains_key`/`get`, ran up to three binary searches over its
  sorted row sections, ~25 string comparisons for a user type name;
- `is_known_type_constraint` was a `matches!` over ~200 spellings;
- `is_native_int_type` was a linear `contains` over 21.

A typed declaration of a user type inside a method (`my Tag $t = $x`) asked
these about ten times per execution. All three are now open-addressing hash
tables computed at compile time (`src/static_str_set.rs`,
`src/runtime/builtin_class_catalogue_index.rs`): one pass over the name's
bytes and about one comparison, with the tables in `.rodata` and no start-up
work.

Per call of `method m($x) { my Tag $t = $x; 1 }` (callgrind, profiling build):
33,194 -> 27,887 instructions (-16%); `my Str $t = Nil` 25,269 -> 23,286. A
warm YAMLish `load-yaml` of the 60-row bench document: 105.5M -> 104.9M (-0.6%).
Refs #12917.
