# JSON::Class aggregate binds and anonymous object-hash keys now work

JSON::Class unmarshalling can bind a computed array or hash into an attribute
slot without itemizing the aggregate. The bind-index path now preserves the
aggregate container and keeps the binding live.

Object-hash attributes with an inline subset key constraint also retain that
constraint when their empty container is initialized. Valid keys can be read,
and invalid keys are rejected through the ordinary subset type checker.

The behavior is covered by
`t/vm/binding/bind-computed-aggregate-hash-value.t` and
`t/types/enum-subset/inline-subset-object-hash-key.t`, based on JSON::Class
0.0.21. Trait re-export support (`is marshalled-by` / `is unmarshalled-by`)
remains tracked separately in #8121.
