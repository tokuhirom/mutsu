# Core classes' own constructors show up in `.^method_table`

`Proc::Async.^method_table<new>:exists` was `False` in mutsu and `True` in rakudo
([#9340](https://github.com/tokuhirom/mutsu/issues/9340)). Test::Mock's `mocked(Type, ...)`
walks `.^mro` and each class's `.^method_table` and overrides every entry it finds. Since
`new` was not listed, it was never overridden, and `$m.new('perl6', '-e', 'say 42')` reached
`Mu.new`, which died with `Default constructor for 'Anon' only takes named arguments`.

mutsu describes each core class in `src/runtime/runtime_init.rs` with a list of the methods
it implements natively. `.^method_table` and `.^methods(:local)` are built from that list,
and `new` was never on it. It is now listed for Proc::Async, IO::Path and Lock, the classes
here whose rakudo counterpart declares its own constructor. Supplier and Channel inherit
theirs from `Mu`, and Promise's is a submethod, so none of those three gets one in the table.

Two lookups were also wrong:

- `.^find_method` answered a core class's native method with a bare name string, so
  `Lock.^find_method('new')(Lock)` died with "No such method 'CALL-ME' for invocant of type
  'Str'". It now returns the same callable Method object `.^method_table` hands out.
- Classes without a constructor of their own could not find the `Mu.new` they inherit:
  `Supplier.^find_method('new')` was false. `.^lookup` never looked at native methods at all.
  Both now share one fallback.

Not covered: core types with no entry in that registry at all (Date, Int, Str, Mu) still
report an empty `.^method_table`. That is a broader gap than this constructor fix.

Pinned by `t/oo/mop/core-class-own-constructor-introspection.t`.
