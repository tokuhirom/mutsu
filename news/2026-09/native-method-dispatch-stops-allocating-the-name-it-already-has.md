# Native method dispatch stops allocating the name it already has

`native_method_0arg` is the entry point for every zero-argument native method
in the interpreter — `.elems`, `.chars`, `.keys`, `.Str`, all of them. Its
first line was:

```rust
let method = method_sym.resolve();
let method = method.as_str();
```

`Symbol::resolve` is `self.as_str().to_owned()`. Interned strings are
`Box::leak`ed precisely so a `Symbol` can hand out a `&'static str` without
holding the table lock — so that pair heap-allocated a copy of a string the
symbol table already owns, and freed it again, on **every native method call**.

Three more of the same sat on the VM's native dispatch path in
`vm_native_dispatch.rs`, all feeding `==`, `starts_with` and `matches!`
comparisons that never needed an owned `String`.

Replacing the four with `as_str()` takes **393 instructions off every native
0-argument method call**.

## Measurements

Baseline `059abe84`. `--profile profiling` builds of the same tree with and
without the change, first run after each build discarded.

| | before | after | |
| --- | ---: | ---: | ---: |
| `@a.elems` loop, 100k | 1,050,693,833 | **1,011,402,253** | **-3.74%** |
| `$o.m()` loop, 100k | 2,401,448,104 | 2,401,455,352 | +0.00% |
| `f()` loop, 100k | 940,397,943 | 940,399,876 | +0.00% |

A user-class method call does not move: it resolves through the class's method
table and never reaches `native_method_0arg`. A sub call does not either.

## How it was found, and what it says about the rest

By filling in a gap in the previous entry's table. That one measured a method
call on a **user class** at 315x and concluded the call is where the gap is.
It did not measure a method call on a **builtin** receiver — which is what real
Raku code, `JSON::Fast` very much included, actually does:

| | mutsu ns | raku ns | ratio |
| --- | ---: | ---: | ---: |
| `$s.chars` | 758.0 | 10.7 | 71x |
| `$s.substr(0,3)` | 857.3 | 59.8 | 14x |
| `@a.elems` | 1052.4 | 10.8 | **97x** |
| `%h.elems` | 1032.7 | 10.0 | **103x** |
| `$n.abs` | 513.0 | 37.6 | 14x |
| `$o.m()` (user class) | 2945.5 | 10.0 | 296x |

`@a.elems` is **8,314 instructions** to answer how many elements an array has.
The native fast path is reached — `native_method_0arg` and `dispatch_core` are
both in the profile — but getting there costs about 7,000 instructions, and the
allocation removed here is only 393 of them.

The rest has a shape worth recording. `view_kind`, the NaN-box tag decode, runs
**57 times per `@a.elems`**:

| caller | per call |
| --- | ---: |
| `Value::view` | 22 |
| `dispatch_core` | 15 |
| `native_method_0arg` | 10 |
| `dispatch_core_numeric::dispatch` | 3 |
| `value_type_name` | 3 |

That is a cascade of `match value.view() { ... }` arms, each re-decoding the
same receiver, walking down a chain of candidate method families until one
claims the name. The receiver's kind is known after the first decode; the other
56 are the dispatch structure asking again.

So the builtin-receiver path has its own version of the problem
[#8880](https://github.com/tokuhirom/mutsu/issues/8880) describes for user
classes, and a different fix: decode the receiver once and dispatch on
`(kind, method)` through a table, rather than a linear cascade of view-matching
families. Filed as
[#8888](https://github.com/tokuhirom/mutsu/issues/8888), separately from #8880
because a per-call-site method cache keyed on a user class's type id would not
touch this path — and `JSON::Fast` is made of these calls, not of user-class
ones.
