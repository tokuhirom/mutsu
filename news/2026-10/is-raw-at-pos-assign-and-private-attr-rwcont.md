# `is raw` AT-POS accepts a subscript assignment; `$!attr := $var` stays a writable container

Two gaps in annotations::containers' `Buffer`/`Binder`, found while working the `annotations` dist:

- `method AT-POS(Int:D $pos) is raw { @!inner.AT-POS: $pos }` returns a `Proxy` bound in an inner array.
  `@buf[0] = 42` did nothing: only an `is rw` accessor was routed through the element-container store.
  `is raw` hands back the container as well, so it takes the store now.
- `Binder.STORE` picks assign-or-bind with `nqp::isrwcont($!value)`. After `$!value := $variable` the
  attribute holds the variable's container and is writable; mutsu treated every `:=` of a private
  attribute as a bound value (the stack carries only the value), so the check was always False. A bind
  whose source is a named, non-readonly variable now keeps the attribute a container; a bound literal
  still does not.

The dist still differs (`+@values is raw` does not keep the argument containers, filed separately).
