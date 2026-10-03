# Text::CSV rows parse a further 19% faster

Refs [#9494](https://github.com/tokuhirom/mutsu/issues/9494).

A second round on the Text::CSV parse loop that `use Services::PortMapping`
runs over 15,000 lines while it loads. The first round
(`method-calls-stop-flattening-the-callers-scope.md`) took a parsed row from
15.27M to 12.47M instructions; this one takes it to about 10.1M. Each item
below is a general mechanism, measured on its own against the row and
against a micro-benchmark of the Raku idiom it serves:

- **`current_package` is one interned symbol.** It was an
  `Arc<RwLock<String>>` with an atomic symbol mirror, so every package switch
  on a method call allocated and every save cloned.
- **A map block with its own signature runs compiled.** `@f.map(-> \x --> Str
  { x.Str })` called each element through the carrier `call_sub_value`, which
  re-evaluates the body from its AST under a rebuilt environment. A block
  with parameters binds the element to them, never to `$_`, so it is now
  called through `vm_call_on_value` like any other closure (13 elements:
  1.36M -> 0.62M instructions). The binder also stopped asking every
  parameter whether its argument does `PositionalBindFailover`; only `@`
  parameters use the answer.
- **A typed bind of an array element** (`my Str $chunk := @ch[$i]`) is
  accepted by the value the bound cell carries instead of walking the
  role/MRO gauntlet for the wrapper (29k -> 23k instructions).
- **Hyper user-method calls on objects** (`self.fields».Str`) dispatch each
  element as a value receiver rather than parking it in a temporary env
  binding and running the by-name mutating dispatch.
- **`.elems` / `.end` on an array** (`$i < @ch.elems`) answer before the
  `CallMethodMut` receiver probes (7.3k -> 4.4k instructions).
- **The constructor lane** now also serves classes whose user `new` declines
  the call, and the declared-type seeds of typed attributes are memoized on
  the class's constructor plan.
- **Typed arrays grow on the native mutator path** (`@!fields.push`) after
  the element type check.
- **`self` inside a block nested in a method** is read from the env before
  any term probe. This is also a fix: a file-scope `constant self` used to
  shadow the invocant there.
- Smaller cuts: a smiley'd type name (`Int:D`) skips the term namespace's
  package-chain probes, the scalar-store fast path asks its default/readonly
  lanes of the store at hand rather than as program-wide latches, and the
  sink/`Failure` checks compare class names without allocating.

Out-of-scope finding filed on the way: a `\x` block parameter and a later
`$x is rw` block parameter collide on one env key
([#11429](https://github.com/tokuhirom/mutsu/issues/11429)).
