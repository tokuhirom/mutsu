# A repeated `.?missing` no longer builds an exception each time

`$obj.?name` answers `Nil` when the invocant has no method `name`. mutsu
answered that by running an ordinary dispatch and swallowing its
`X::Method::NotFound`. So every miss built the whole exception, including the
"Did you mean" walk over the class's methods and the word-wrapped message, and
then threw it away. XML::Class asks `self.parent.?xml-namespace-prefix` once
per XML node (#17987).

The first argument-less `.?name` miss on an instance of a user class still
runs the dispatch. When the error proves that the lookup itself missed (an
`X::Method::NotFound` for this method on this type), the `(class, method)`
pair is recorded for the registry's method generation. Later calls answer
`Nil` at the start of the call opcode (`vm_maybe_call_miss_lane.rs`).

Anything that can make the method appear invalidates the record or never
reaches it:

- `augment` and `^add_method` change the method generation;
- a mixin is not a plain instance, so it never reaches the lane;
- `.^add_fallback` keeps the lane shut while any fallback exists;
- `FALLBACK`, `handles` and a user `^find_method` answer the call, so it is
  never recorded.

10000 `.?missing` calls on a user-class instance go from about 141,000 to
about 2,000 instructions each.
