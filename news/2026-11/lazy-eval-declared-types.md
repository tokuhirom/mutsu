# The compile-time parameter-type check reads `use`d modules only when it must

Before the mainline runs, mutsu checks every routine's parameter and return
types (`sub yoink(Junctoin $barf)` is `X::Parameter::InvalidType`). Because
the `use`d modules have not loaded yet at that point, the check collects the
types they declare by scanning their source -- and it did so for every
program, whether or not any routine named a type. For `use Test; ok 1;` that
scan of `Test.rakumod` was 2.29M of the 37.6M instructions the module load
costs on a warm precompilation cache.

The declared types are now collected on the first routine that actually
constrains a parameter or its return type; a program whose routines name no
types (or that declares none) skips the scan. `use Test` load, callgrind,
profiling build, paired A/B: **37.59M → 35.30M** Ir (#12026).
