# Positional nqp ops refuse a high-level List, as Rakudo does

The positional `nqp::` ops are REPR operations: `elems`, `atpos`, `bindpos`,
`push`, `pop`, `shift`, `unshift`, `splice`, `setelems`, `existspos` and
`iterator`, with their native forms. Rakudo runs them on a `VMArray`, which is
a raw `nqp::list`, a `$!reified` view, a native `array[T]`, a `Buf`, a `Uni`
or a `CArray`. On a high-level `List`, `Array`, `Str`, `Seq`, `Range` or
`Capture` it dies:

```
This type (Array) does not support elems
This type (List) does not support positional operations
Cannot iterate object with P6opaque representation (Hash)
```

mutsu used to answer them on anything list-shaped. Now every positional op
asks one REPR check (`runtime/nqp_repr_check.rs`) at the nqp dispatch entry
before its own arm runs (#12671). A `Hash` / `Map` still answers `elems` and
the associative ops, which Rakudo delegates to its `$!storage`.

Making the check strict exposed places where mutsu itself handed out a
high-level list where Rakudo hands out a raw one:

- `my $l = nqp::list(...)` converted the raw list into a `List` on the
  assignment. Assignment keeps it raw now; binding a parameter still hllizes.
- `nqp::radix`, `split`, `uname`, `backtrace`, `backtracestrings`,
  `getsignals` and `decodelocaltime` now answer a raw `VMArray`.
- `getenvhash`, `backendconfig` and `capturenamedshash` now answer a raw
  `VMHash`.
- `nqp::create` of an `is repr('VMArray')` / `is repr('VMHash')` class now
  makes a raw collection.
