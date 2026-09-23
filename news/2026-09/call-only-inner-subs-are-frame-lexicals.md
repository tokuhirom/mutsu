# A call-only inner `my sub` no longer touches the routine registry

A `my sub` declared inside a routine body was made lexical by writing it into the
program-global routine registry on every call of the enclosing routine — twice, once for the
hoisted registration and once in sequence — and rolling the whole registry back on return.
The declaration also set `has_inner_subs`, which kept the enclosing routine off every light
call path, and each call to the inner sub resolved its name through a registry whose
generation had just moved. JSON::Fast's `unjsonify-string` declares `fetch-codepoint` this
way ([#9103](https://github.com/tokuhirom/mutsu/issues/9103)).

[ADR-0113](../../docs/adr/0113-frame-lexical-inner-subs.md) binds such a sub as a *frame
lexical* when the body provably does nothing with it except call it by its bare name. A
compile-time pass (`src/compiler/frame_lexical_routines.rs`) proves that from the body's
serialized AST and its bytecode, marks the declaration plans, and lists the routine in the
`lexical_routines` table of every chunk that calls it. At run time the declaration derives
the routine's definition once per interpreter through the ordinary registration (rolled
back immediately) and afterwards is one table probe; the four bare-call handlers consult the
table before any name-keyed resolution and dispatch straight to the compiled body. The
enclosing routine now takes the positional-light path and takes no registry snapshot.

Measured with callgrind on a `--profile profiling` build, second run after warming the
precompilation cache, differencing two input sizes:

| workload | before | after |
| --- | ---: | ---: |
| `from-json` of `"a/b"` strings (the #9103 repro), Ir per string | 541K | 444K (-18%) |
| routine declaring a one-line inner sub it calls twice, Ir per call | 71.4K | 19.8K (-72%) |

The same routine with the inner sub hoisted to file scope costs 12.7K per call.

An inner sub used as a value (`&name`, returned, passed along) still takes the registry
path; giving it a frame-lexical code object is the next slice. Pinned by
`t/vm/scope/frame-lexical-inner-sub.t`.
