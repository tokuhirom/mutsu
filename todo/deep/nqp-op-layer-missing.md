# mutsu has almost no `nqp::` ops, which blocks the lizmat-style dists

Found 2026-07-25 while re-running the real-dist compatibility sweep (PLAN §B4).
`String::Utils` — a widely depended-on distribution — fails to load with:

```
Runtime error: An exception occurred while evaluating a CHECK
  Unknown function: unipropcode
```

The trigger is a module-scope constant, so it fires at load time:

```raku
my constant $gcprop = nqp::unipropcode("General_Category");
```

## What is actually missing (measured, not estimated)

mutsu maps `nqp::foo` to a plain function named `foo` (a handful of ops are
special-cased by full name in `src/runtime/builtins.rs`: `nqp::atkey`,
`nqp::atpos`, `nqp::ordat`, `nqp::gethostname`, `nqp::bindattr`). Everything else
resolves as an ordinary builtin, which is why some ops happen to work — `chars`,
`concat`, `index`, `substr`, `join`, `split`, `flip`, `elems` collide with real
Raku builtins of the same name.

`String::Utils` alone uses 59 distinct `nqp::` ops. Probing each one
(`mutsu -e 'use nqp; nqp::<op>()'` and looking for `Unknown function`) says **53
of the 59 are missing**:

```
add_i atpos_i atpos_s bindattr_i bindattr_s bindkey bindpos_s bitand_i
bitshiftr_i box_s chars clone concat const create deletekey eqaddr eqat existskey
findcclass findnotcclass flip getattr getattr_i getuniprop_int hllbool if iseq_i
isgt_i isle_i islt_i isne_i isnull isnull_s istype list_s mod_i not_i null null_s
push_i push_s setelems sha1 stmts strfromcodes strtocodes sub_i substr
unipropcode until while x
```

(The probe reports a name as present when the failure is about arguments rather
than resolution, so the list is a lower bound on what needs real semantics —
`chars`/`substr`/`concat` "exist" only as their Raku namesakes, whose signatures
and coercion rules are not the nqp ones.)

## Why it is a deep problem, not a ticket

- **It is a whole compatibility layer, not a bug.** These ops are a typed,
  low-level ISA: `_i`/`_s`/`_n` suffixes mean native int/str/num operands with no
  boxing, `nqp::if`/`nqp::while`/`nqp::until`/`nqp::stmts` are *control
  structures* taking thunks (they cannot be ordinary functions at all),
  `nqp::null` / `nqp::isnull` expose a null sentinel that mutsu's `Value` has no
  representation for, and `nqp::const::*` is a constant namespace, not a call.
- **Several ops need representation work.** `nqp::create`/`getattr`/`bindattr`
  operate on uninitialised P6opaque storage; `nqp::box_s`/`nqp::decont` straddle
  the native/boxed boundary; `array[uint32]` buffers (`nqp::strtocodes`,
  `nqp::strfromcodes`, `nqp::push_i`) want native-typed arrays.
- **The ISA is large and open-ended.** Fixing one dist's 53 ops does not settle
  the next dist's set, so the design question is "which subset, with what
  semantics, dispatched how" — a decision worth an ADR rather than a pile of
  ad-hoc builtins. In particular, folding nqp ops into the ordinary builtin
  namespace (today's implicit design) is what makes `nqp::chars` silently mean
  Raku's `chars`; a separate `nqp::` dispatch table would be the honest shape.

## Deliberate note on scope

This is *not* the same axis as the "compiler guts" skip in the sweep
(`QAST`/`NQPHLL`/`EXPORTHOW`/`Metamodel::Primitives`), which is genuinely
out of reach. `nqp::` ops are ordinary low-level operations with well-defined
semantics; implementing a chosen subset is tractable and would unlock a
meaningful slice of the ecosystem, since lizmat's modules reach for them freely
for speed.

## Affected files

- `src/runtime/builtins.rs` — the five full-name `nqp::` special cases
- `src/compiler/stmt.rs`, `src/runtime/runtime_module.rs` — `use nqp` is accepted
  as a no-op pragma
- `src/vm/vm_var_get_ops.rs` — `nqp::gethostname` as a term

## Repro

```
$ mutsu -I <String-Utils>/lib -e 'use String::Utils'
Runtime error: An exception occurred while evaluating a CHECK
  Unknown function: unipropcode
```
