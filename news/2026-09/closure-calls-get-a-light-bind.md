# A pointy block's call stops paying the general signature binder

A named sub whose signature is plain positionals binds its arguments straight into locals slots
(`call_compiled_function_positional_light_at`). A *closure* had no equivalent: every call through a
`Sub` value went to `call_compiled_closure_in_unit`, which handed the arguments to the general
signature binder — `bind_function_args_values` — whatever the signature looked like.

For a `-> $a { $a }` pointy block that binder can only reach **one** of its arms. A single-parameter
pointy block carries no `ParamDef` at all: its signature survives as the parameter *name* in
`SubData::params`, so it falls into the legacy placeholder path's "plain positional identifier"
branch, whose entire content is "itemize the argument and store it under the parameter's key".
Reaching that branch cost four `Vec`s (`filtered_args`, `plain_args`, the single-argument-rule
re-filter, `positional_args`) and two `@_` arrays — 4 191 of the 11 952 instructions the call cost
after [the #8302 fix](closure-call-stops-re-interning-its-own-names.md) had taken the per-call
interning out of the same path.

## The light bind

`src/vm/vm_closure_light_bind.rs` is the direct route to that arm. It is not a second binder: it
reproduces the arm's observable effects and **declines** — so the general binder runs, with its own
messages and diagnostics — for everything it is not a faithful substitute for.

The signature half of the gate is settled once per code object, as a `light_bindable` flag on the
`ParamNameSyms` #8302 already builds there, so it shares that struct's `OnceLock` and its
pre-interned parameter symbols rather than adding a mechanism of its own. It admits a signature made
entirely of bare identifiers with no `ParamDef`. Every other spelling has a branch of its own that
the light bind does not reproduce, and each is rejected for a named reason — `^a`/`:a` placeholders
consume named arguments and publish twigil-less aliases, `@x`/`%x`/`&x`/`\x` bind raw (and `@x`
re-homes its value), `_` is the topic (exempt from itemization and bound by the implicit-topic
machinery), `@_`/`%_` are the aggregates the binder publishes itself, `self` is mirrored onto the
reserved lexical key, and a `__mutsu`-prefixed synthetic name (the `supply` block emitter) is
itemization-exempt. That also keeps `WhateverCode` out: `*+1` curries to a lambda whose parameter is
`_`.

The argument half is checked per call: exact arity only (a short or surplus call is the binder's to
diagnose, and this path has no value to give an unbound parameter), and no `Pair`/`ValuePair`
argument, whose positional-ness depends on the rest of the signature.

## Numbers

The #8335 benchmark (`-> $a { $a }` against `sub named($a) { $a }`, 100 000 calls, release,
`MUTSU_JIT=off`), and callgrind on a 20 000-call reduction of it:

| | before | after |
| --- | --- | --- |
| closure call, wall clock | 0.235 s | 0.197 s (−16%) |
| closure call, Ir/call | 11 952 | 9 176 (−23%) |
| the general binder, Ir/call | 4 191 | 0 |
| whole program, Ir | 350.2 M | 290.1 M (−17%) |
| ratio to the named sub (2 291 Ir/call) | 5.2x | 4.0x |

## What is still open

[#8335](https://github.com/tokuhirom/mutsu/issues/8335) names two costs; this is the first. A
closure call still pays an **env frame** where the named light path pays none: `push_call_frame` /
`push_caller_env` / `pop_call_frame`, a scoped-child `Env` created and dropped, and the
block/routine stack pushes — together ~3 000 of the remaining 9 176 instructions. And a
*multi*-parameter pointy block (`-> $x, $y { … }`) does carry `ParamDef`s, so it still takes the
general binder's full path; admitting that shape means reasoning about the branches a `ParamDef`
signature can reach (the scalar-container share, the source variable's type constraint), which is
the `CompiledFunction`-shaped signature description that issue sketches as its option 1.
