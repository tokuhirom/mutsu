# Computed-name methods see the file's imports; `clone` shares initialized `@`/`%` attributes

Two gaps found with the `JSON::RPC` distribution's client (`t/00-client.rakutest`).

- A method with a computed name (`method ::('rpc.flush') { ... }`) was compiled only when the
  class registered, without the lexical import chain of the surrounding `use`s, so an imported
  sub such as `to-json` died with "Unknown function". The class-body pass now compiles it like
  any other method.
- `clone` copied every `@`/`%` attribute. Rakudo keeps the container of an attribute that has a
  default initializer (`has @!stack = ( )`, even empty) or is already filled, so a clone shares it;
  only a bare, unfilled `has @!x` is vivified separately by each. The interpreter's clone (with or
  without `:attr(v)` twiddles) now follows that; `Mu.clone` on an instance declines to it because
  it needs the class's attribute declarations.
