# Compiler-minted names come from a compile session

The compiler mints names and ids that must be unique in the process:
- the `Pkg::&<closure>/N` package that scopes a closure's `state` variables;
- hidden locals such as `__do_decl_init_N`;
- the site id of an `END` phaser;
- the serial that keeps two identical lexical subs' aliases apart.

They came from process-global counters. A value minted that way means
nothing outside the process that minted it. That is the first obstacle to
storing compiled bytecode in the precompilation cache
([ADR-11756](../../docs/adr/11756-compiled-bytecode-precompilation.md)).

They are now minted as (session, ordinal), packed into one integer. The
ordinal counts up within one top-level compile, and nested compiles and
sub-compilers share it. The session is chosen per top-level compile, for now
from a process-global counter. So every compile still mints values that no
other compile in the process shares, and nothing observable changes.

The next step gives a cacheable module compile a content-addressed session. Its
values are then the same in every process, and a cached chunk can be reused as
is.
