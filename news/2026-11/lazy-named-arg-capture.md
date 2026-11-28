# `:$var` arguments to a constructor no longer build a shared cell

A named argument naming a caller variable (`Quoted.new(:$value, :$tag)`) has
to hand its callee the variable's container: an `is rw` or `is raw`
parameter, a `*%h` slurpy and a `|c` capture all write through to it, as
they do in rakudo. `MakeNamedArg` built that container at the argument, for
every callee: it promoted the variable's slot to a shared cell and registered
the variable's type constraint and default on the cell, about 4k instructions
per argument per call. The most common callee, the default constructor, only
copies the value into an attribute.

The argument of a method call now travels lazily: the Pair keeps the
`WrapVarRef`-tagged value, and the call site resolves it before anything else
reads the arguments (`src/vm/vm_lazy_named_args.rs`). When the call is a
`.new` that the constructor lane answers for a class that runs no user code
over the arguments (no user `new`, `bless`, `BUILD`, `TWEAK`, replaced
BUILDPLAN or `Attribute.set_build`) and binds no attribute
`is built(:bind)`, each argument takes the variable's current value. Every other callee, and a constructor call the lane declines,
gets the shared cell exactly as before.

On the reproducer of #19384 (`Quoted.new` with two `:$var` and two literal
named arguments, a class composing roles with typed `is required`
attributes), callgrind on the profiling build:

| | Ir per call | allocations per call |
| --- | ---: | ---: |
| before | 39.4k | 40 |
| after | 32.3k | 28 |

and 3.9 µs to 2.7 µs per call on the same build.

What remained of a lazy argument's cost in `MakeNamedArg` was mostly copying
the key: a named Pair stored its key as its own `String`, so every named
argument paid a `malloc`, a copy and a `free` for a constant. A Pair built
from a flat `Str` key now shares that string (`PairKey::Shared`). That brings
the reproducer to 30.9k Ir and 24 allocations per call, and a `:$var`
argument to about 295 Ir in `MakeNamedArg`.
