# `my $x := EXPR` declares without the general store

A `$` scalar's `:=` declaration used to run the general `SetLocal` cascade:
by-name env probes, the Proxy/Seq/decont/alias/array-share arms, and an env
walk looking for other bindings of the name. That cost ~7.7k instructions
more than `my $x = EXPR`, and a `my Mu $x := EXPR` paid another ~7k for type
ops whose constraint can never fail.

Three changes, for ADR-18244 stage B (#18274):

- **The bind-declaration lane** (`exec_set_local_bind_decl_fast`). A slot
  that is its variable's only home, bound to a value with no container of its
  own (an `Int`, `Str`, object or code value), gets exactly what the cascade
  left behind for that shape: the name's readonly mark, and the value seated
  in its readonly binding cell (ADR-11142). Containers, Proxies, aggregates,
  list-like values, `Nil` and type objects keep the general store.
- **`my Mu $x := EXPR` compiles as `my $x := EXPR`.** Every value binds to
  `Mu`. After the bind, `.VAR` is the bound value or container, and a later
  rebind checks against `Mu`, as an untyped `$x` already does. So the declared
  type cannot be observed, and its `SetVarType*`/`TypeCheckBind` ops are
  gone.
- **A container-requesting call takes the early plain-method lane.** The
  right side of `:=` flags its method call as a container request. The early
  lane refused every such call, so it walked the whole probe stretch before
  reaching the late gate, which already keys the call with a want-ref marker.
  The early lane now builds the same key.

Per construct, in a method body (callgrind slope, profiling build):

| construct | before | after |
| --- | ---: | --- |
| `my $v := self.foo` | 31.3k | MEASURE |
| `my Mu $v := self.foo` | 38.7k | MEASURE |
| `my Mu $b := SomeType` | 29.8k | 22.3k |
| `my $v = self.foo` (reference) | 23.6k | 23.6k |
