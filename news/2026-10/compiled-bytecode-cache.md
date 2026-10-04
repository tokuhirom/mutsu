# Module mainlines can be served from a compiled-bytecode cache

ADR-11756 step 4 adds the compiled section to the precompilation cache. It is
off by default. With `MUTSU_PRECOMP_BYTECODE=1`, the first load of an eligible
module writes its mainline compile next to the cached AST (`{hash}.code`), and
the next process decodes it instead of compiling. The entry is used only when
all of these hold:

- the compile context matches;
- the AST about to be compiled is the one that was compiled (a stable hash of
  the statements);
- every question the compile asked of state outside its AST
  (`compiler::compile_inputs`) still gets the same answer;
- the process has not already compiled that module.

`MUTSU_PRECOMP_VERIFY=1` compiles every hit afresh as well, and stops with
status 70 unless the two encodings are byte-identical. `MUTSU_PRECOMP_TRACE=1`
prints how the cache answered for each module.

Getting the verify mode clean over all of `t/` and the roast whitelist meant
removing every process-local value from a module's compile. These had all been
minted from process counters:

- declaration ids and anonymous names in the parse;
- BEGIN-prologue and package-phaser value slots;
- chained-comparison temps;
- builtin-prelude declarations.

They now come from content-addressed sessions keyed on the module's source.
`StateVarInit`'s key operand is a `Symbol` that the codec maps, rather than a
raw id. An AST cache entry records the parse session it was minted under.
`ast::stable_hash` combines map entries without regard to order and skips
object ids. That also fixed a spurious "Redeclaration of routine" for a module
routine whose body holds a Signature literal.

Measured on `use Test; ok 1;` minus an empty script (release build, warm cache,
callgrind): **84.9M instructions without the compiled section, 64.7M with it
(-23.8%)**. Of what remains, routine registration is 23.5M and decoding 8.7M.
Registration is phase 2 of the ADR. Turning the cache on by default is step 5.
A module holding a Signature literal is not cached yet (#11841).
