# Loading a module no longer scans the routine registry per exported name

Every `t/` and roast file starts with `use Test`, so what a module load costs is paid thousands of
times per suite run (#11756). Several steps of the load asked "which registry keys belong to the
multi family `Pkg::name`?" by scanning every key of the function registry:

- `import_module` did it up to five times per exported name (the family itself, its method
  candidates, the `EXPORT::ALL` and `GLOBAL` fallbacks);
- the hoist bookkeeping and the sub-versus-multi redeclaration check did it once per routine
  registration.

They now go through `FunctionTable::family_keys`, which probes only the names the interned-name
family index lists for that family (the index #11761 introduced for export aliases), and re-checks
each one against the registry, so the answer is the same set the scan found.

Three line-by-line source scans that ran on every load now start with a substring test that rules
out the common case: the `no precompilation` directive check (run twice per load), the `need`
dependency scan, and the pod directive check of `$=pod` collection. Removing leaked `MAIN`
routines tests for `MAIN` before splitting each registry key.

Load cost of `use Test; ok 1;` minus an empty script (profiling build, warm precompilation cache,
callgrind instructions): **53.6M → 45.3M**. #11756's goal is 20.6M; the remaining cost is mostly
routine registration, decoding the compiled section and the AST-side analyses a cache hit still
runs.
