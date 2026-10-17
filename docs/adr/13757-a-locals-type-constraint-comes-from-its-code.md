# ADR-13757: A local's type constraint comes from the code that declares it, not from a by-name record

- **Status**: Accepted (2026-10-17, by tokuhirom; Proposed earlier the same
  day). Implementation follows §3; record each step's progress in §3.
- **Date**: 2026-10-17
- **Related**: [ADR-12529](12529-frames-resolve-names-through-the-static-link.md)
  (frames resolve names through the static link; its §3 records slice 45, the
  failed attempt that motivated this ADR),
  [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md) (locals are
  slot-addressed), [#13746](https://github.com/tokuhirom/mutsu/issues/13746)
  (an EVAL'd store inside a sub misses the program variable's own type)

## 1. Context

A typed declaration (`my Int $x`, `my Str @a`, `my %h{Str}`) records its
constraint at run time as an env entry keyed by name: `SetVarType`
(and its scoped/hoisted forms) writes `__mutsu_type::x` (and, for a hash, a
key-type record). Every store then asks for that record by name:
`var_type_constraint` and its `_sym` / `_value_sym` / `_value_for` variants
and `var_hash_key_constraint` have about 120 call sites, and all of them end in
`declared_meta_lane` → `env().get_sym`.

A by-name walk answers "some variable called `x` somewhere up the chain". It
cannot say which variable that is:

- When the running frame has no record, the walk continues into the frames that
  **called** the routine, and a caller's unrelated same-named variable
  constrains the store. ADR-12529's eighth census found this probe to be the
  largest group of lookups that still resolve in an intermediate caller frame.
  Wrong answer on main:

  ```raku
  my @a;
  sub push-it() { EVAL q[@a.push('s')]; @a.elems }
  sub typed-caller() { my Int @a; push-it() }
  say typed-caller();   # raku: 1; mutsu: type check failure
  ```

- ADR-12529 slice 45 tried to make these records follow the static link like
  the variable. Each narrowing broke another shape: a module routine's own
  untyped `$desc` reached a mainline `my Array $desc`; a fresh `my $path` (not
  yet bound when its declaration probes the type) reached a caller's
  `my URI::Path $path`; a destructuring `my (..., $uri)` reached a `URI:D $uri`
  record in a flattened tier the link reaches. A sigilless parameter installs
  its alias source name in the callee frame, so "the frame binds the name" is
  not decidable from the env either.

The compiler, however, knows the answer for every local: it compiled the
declaration, so it knows the slot and the constraint (`SetVarType`'s
`tc_idx` is already a constant of the declaring code).

## 2. Decision

**A store to a local of the running code takes the local's constraint from the
code. Only a store to a name the code does not declare looks a record up by
name, and it looks where the variable resolved.**

1. The compiler records, per local slot, the declared constraint: the type
   constraint string and the hash key constraint, or none for an untyped
   local (`CompiledCode::local_constraints: Vec<Option<LocalConstraint>>`,
   indexed like `locals`). A `my` without a type gets `None`. That is the
   positive fact which the by-name walk could never produce, so the record
   probe stops falling through to someone else's variable.
2. The readers take the slot when they have one. A store through `SetLocal` /
   `SetLocalDecl`, the element and compound stores on a local, and the
   push/append family on a named local all hold `code` and the slot. They read
   `code.local_constraints[slot]` and never consult the env.
3. The by-name record stays for names the running code does not declare:
   a free variable of a closure or routine, an `EVAL`'d or symbolic store, a
   `state`/`our` name resolved through a package. Its lookup goes through the
   same resolution as the variable's value (the static link, ADR-12529),
   not through the caller frames. The record is written next to the variable
   (the declaring frame's tier, or the unit lane for a program-scope variable,
   which also closes #13746).
4. A runtime rebind that changes the constraint (`:=` binding a typed
   container, `runtime_var_bind_meta`'s `Pair` override) keeps its by-name
   override, consulted after the code's constraint only for that slot.

## 3. Migration

1. Add `local_constraints` to `CompiledCode` (and the precomp codec), filled
   where the compiler emits `SetVarType*` for a slot-resolved local. No reader
   changes; a debug assertion compares it against the by-name record at every
   `SetLocal` to find the declarations the compiler does not see.
2. Switch the local-store readers (`exec_set_local_op_inner` and the
   `vm_var_assign_*` element/compound paths) to the code's constraint.
3. Switch the push/append/unshift family and `vm_call_method_compiled_cache`.
4. Restrict the by-name record lookup to non-locals, through the static link;
   the caller-frame walk for `__mutsu_type::*` goes. Census: re-run the
   ADR-12529 caller-frame census and check that `declared_meta_lane` no longer
   appears.

Each step runs the full gate and the battery suite (the URI and
HTTP::UserAgent files caught slice 45's regressions).

### Implementation status

- **Steps 1, 2 and 4, first slice** (`fix/13757-local-type-from-code`).
  - The bake exists already: `binding_desc::DeclaredConstraint`, per scalar
    slot (#8877). It gains `Untyped`, recorded for a scalar `my` without a
    constraint and for an untyped parameter that does not alias the caller's
    container (not sigilless, `is rw` or `is raw`).
  - The type checks of `exec_set_local_op_inner` and the native store fast
    paths read it (`vm_slot_type_constraint.rs`). A slot the code declares
    untyped never probes the by-name record.
  - With the running code's own untyped locals out of the probe, ADR-12529
    slice 45's rule (a record follows the static link when the frame does
    not bind the variable and the program scope does) no longer meets the
    shapes that failed it: Test's `proclaim` `$desc`, URI's fresh
    `my $path`, HTTP::UserAgent's destructured `$uri`. The rule lands here,
    which fixes §1's example.
  - Still on the by-name record, for later slices: the `@`/`%` slots (the
    bake stays scalar-only), the element, compound and push-family readers,
    and `vm_call_method_compiled_cache`.
- **Step 4, second slice** (`fix/13757-array-hash-slot-constraint`). The
  element stores read the by-name record, and two shapes still reached a
  caller's record:
  - An element store into a captured variable seeds the cell's container
    under its name in the running frame, so the frame looked like it bound
    the variable. A record now takes the static link when the frame's
    binding is the program scope's own container (`same_variable`).
  - A closure's frame root has no static link, so its record lookup walked
    on into the frames that called it. A closure whose capture holds the
    variable now ends the record lookup at that capture.
  - This slice changes where the record is looked up, not who reads it:
    element stores, the push family and the compound stores still read the
    by-name record, which is correct as long as it resolves where the
    variable does. Moving them to the slot bake is still open.
- **Step 3, argument sources** (`fix/13757-varref-carries-declared-type`).
  A `VarRef` argument was typed by a by-name probe that the binder and the
  multi dispatcher (`multi_arg_type_keys_into`, `unwrap_varref_for_dispatch`,
  `args_matching`) ran from the callee's frame. The caller-frame census put
  most of its `declared_meta_lane` hits there. `WrapVarRef` now resolves the
  type in the variable's own frame (an untyped slot answers from the code),
  and the `VarRef` carries it (`VarRefDeclared`). A `VarRef` built without a
  site resolution still asks by name.

## 4. Consequences

- Correctness: a routine's store is checked against its own variable's type,
  never a caller's. The four shapes in §1 become pins.
- Performance: a typed local store reads a slot-indexed vector instead of
  hashing a metadata key and walking env tiers. An untyped local store reads
  `None` and skips the probe, which today is skipped only through the
  per-name `env_type_constraint_seen_for` latch.
- Cost: every `SetVarType*` emission site must also fill the vector. A
  declaration the compiler cannot see (a runtime-built routine, `EVAL`'d
  declarations) keeps the by-name record, which step 1's assertion
  inventories.

## 5. Alternatives considered

- **Records follow the static link** (ADR-12529 slice 45): rejected. The env
  cannot tell which variable a record belongs to (§1).
- **Stop the walk at the frame root**: rejected. It drops the record a
  sigilless alias or a closure's free variable genuinely needs.
