# A `* + *` call no longer runs the whole signature binder

Calling a WhateverCode like `* + *`, or a pointy block with more than one parameter (`-> $a, $b { … }`), through a value call went through the general signature binder's full `ParamDef` path every time. The parameters are untyped, so most of that work had no effect: two filtered argument vectors, a `String` copy of each parameter name, a `Symbol::intern` per parameter, an implicit-`Any` type check, and the per-parameter type-constraint and readonly passes. That cost about 8k of the ~19.4k instructions a `* + *` call took. #10119 made it visible when it moved sequence generators onto this path (#10702).

The existing light closure bind (#8335) only served signatures with no `ParamDef`s at all. It now has a twin for signatures made only of untyped, plain positional `$` parameters, optionally `is raw`, with no default, `where`, coercion, sub-signature or other trait. That classification, together with each parameter's interned name, itemization mode and readonly mark, is computed once per code object (`ParamNameSyms::light_def_params`). It is not computed per call.

The fast path is a faithful substitute or it declines. It requires exact arity, and every argument must be a plain value of a kind that is always `Any`. It declines a `Pair`, a `VarRef` or call-site source name (an `is raw` parameter aliases a caller variable), a container cell, a `Junction`, a type object and an instance, and the general binder then runs as before with its own diagnostics.

On #10702's benchmark (`(1, 2, * + * ... *)[5]` in a loop, callgrind Ir per iteration, warm, `--profile profiling`) this went from 626,340 to 431,640 instructions: −31%, about 6.1k per generator call. That is well under the 523,436 it cost before #10119. The same saving applies to every multi-parameter block called by value: `.sort` comparators, `.map` with a two-parameter block, `&f(...)`.

While doing this I found that untyped block parameters reject `Mu` and `Junction` arguments because they are checked against `Any` instead of `Mu`. That is filed as #10767.
