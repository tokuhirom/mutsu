# A type constraint's metadata value is interned, not rebuilt per registration

A `JSON::Fast` decode of a 200-record document made about 1.48 M heap
allocations ([#8898](https://github.com/tokuhirom/mutsu/issues/8898)), with
~11.6% of the decode spent inside the allocator itself. This removes a fifth of
them.

## What was being rebuilt

Three sites register a variable's type constraint into its
`__mutsu_type::<name>` env metadata, and all three run *per execution*: the
typed-declaration op (`exec_set_var_type`), the routine-scoped declaration
writer (`set_var_type_constraint_routine_scoped`, the lane a `my int $i` inside
a sub takes), and the parameter binder (`bind_param_type_constraint_sym`).

Each of them ended in `Value::str(<the constraint's text>.to_string())`. The
text is a compile-time constant — `int`, `str`, `Str` — sitting in the
bytecode's constant pool or in the `ParamDef`, and it never changes. But
getting it into the env cost, every time:

- `parse_container_constraint` copying the value type out of the very string it
  was looking at (`raw.to_string()`), plus the key type for an object hash;
- `Value::str` wrapping that copy in a fresh `Arc`;
- `ParamDef::assignment_type_constraint` cloning the `String` off the
  `ParamDef` so the binder could be handed an owned one;
- `exec_set_var_type` calling `into_owned()` on a `Cow` that was already
  borrowing the pool string, to satisfy an `Option<String>` parameter.

So a `my int $i` cost four heap allocations and a typed parameter bind three,
producing an answer identical to the one the previous execution produced. On
the decode profile that family was, per decode: `parse_container_constraint`
92,370, `exec_set_var_type` 66,932, the binder's `String::clone` 84,316,
`set_var_type_constraint_impl` 27,455, `bind_param_type_constraint_sym` 25,438.

## The three changes

**`Interpreter::container_constraint_parts` parses into slices.**
`split_once` / `strip_suffix` / `trim` only ever narrow their input, so the
value and key types always *were* subslices of the constraint text; owning them
was a copy made solely to be read and dropped. `parse_container_constraint` is
now that function plus an `into_owned()` for the callers that keep the answer
past the borrow — the container-tagging path, which is not the hot one.

**`runtime::constraint_meta` interns the metadata `Value`.** The set of
constraint spellings a program contains is fixed at compile time and tiny; the
number of registrations is unbounded. So the `Value::Str` is built once per
spelling and every later registration is an `Arc` refcount bump. This is the
same memoization `MetaNs` applies to the metadata *key*, now applied to its
value. It is sound without qualification: `Value::Str` is an immutable
`Arc<String>` and a GC-free scalar variant, so a shared copy cannot be mutated
through, cannot enter a cycle, and needs no GC root; and the `Symbol -> text`
mapping is append-only, so an entry cannot go stale.

**The setters and the binder take `Option<&str>`.** That is what lets
`assignment_type_constraint` borrow out of the `ParamDef` instead of cloning,
and lets the declaration op keep its `Cow` borrowed. Where the binder genuinely
rewrites a constraint (folding an argument's key type into a `%` parameter's),
the owned form stays.

One smaller fix rides along. `save_type_meta_for_scope_exit` records the
metadata a typed declaration shadows, once per scope — first write wins. It
built the `entry()` key string and cloned the env value on *every* execution,
including the iterations where the record already existed and both were
immediately dropped. A loop body that re-declares on every iteration is exactly
the shape the save exists for, so that was all but the first iteration.

## Measured

`callgrind` on the issue's own workload, differencing two runs that differ only
in how many times `from-json` runs so startup and module compilation cancel;
both sides warm, both from the same `--profile profiling` build recipe,
baseline re-taken from `main` at `71727550`.

| marginal, per decode of 200 records | before | after | |
| --- | ---: | ---: | ---: |
| heap allocations | 1,477,320 | 1,190,011 | **-19.45%** |
| instructions | 2,347,652,670 | 2,306,890,035 | -1.74% |

The six sites above go to zero or near it: `parse_container_constraint`
92,370 → 0, `exec_set_var_type` 66,932 → 0, `set_var_type_constraint_impl`
27,455 → 0, `bind_param_type_constraint_sym` 25,438 → 0, `String::clone`
280,248 → 230,562.

**No wall-clock claim.** -1.74% of instructions is under this box's run-to-run
spread, so a stopwatch here would be measuring noise. What the change does is
strictly less work; whether that is visible in the benchmark series is for the
bench CI to say.

## What is left

#8898 stays open. The largest remaining allocation sites on the re-taken
profile are `var_type_constraint_sym` (91,361 per decode — the `.to_owned()`
copy of a constraint the caller only borrows; `var_type_constraint_value_sym`
already exists for exactly this and the hot callers have not been moved onto
it), `Vec::spec_from_iter_nested` (94,964), `RawVecInner::try_allocate_in`
(54,895), `hashbrown::fallible_with_capacity` (72,113) and
`set_shared_var_sym` (67,535).
