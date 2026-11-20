# The built-in method lane decides its site checks once per call site

A call like `$html.substr($p, 1)` or `$ch.ord` is answered by the built-in
method lane (#11276) when its receiver has a row for the method. On every
call the lane used to re-ask several questions that depend only on the call
site:

- whether any row is named by the method at this arity;
- whether the site is a `map`/`grep`/`first` on an `@` variable, which the
  rw-writeback dispatch must see;
- whether the receiver variable is an `@!`/`%!` attribute;
- whether every argument source is positional;
- for an inline receiver, whether the name is one the full path treats
  specially.

These cost about 300 instructions per call: constant-pool string compares
and an argument-descriptor walk.

They are now computed once per chunk into a per-op verdict
(`CompiledCode::method_site_static`, `vm_method_site_static.rs`), read as
one indexed load (ADR-18357 stage 3). The questions only the running program
can answer are still asked per call:

- pending writebacks;
- a user `find_method`;
- the receiver's shape;
- the registry generation;
- the arguments' types.

Callgrind slopes on a profiling build:

| loop body | before | after |
| --- | ---: | ---: |
| `$s.substr($p, 1)` | 3,833 | 3,527 |
| `$s.substr($p, 1).ord` | 4,891 | 4,455 |
