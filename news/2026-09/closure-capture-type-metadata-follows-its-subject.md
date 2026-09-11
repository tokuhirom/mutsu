# A closure's captured `__mutsu_type::` metadata now follows its subject

The filtered closure capture (`Interpreter::capture_closure_env`) kept every
`__mutsu_type::<name>` typed-lexical metadata key it could see, unconditionally.
It had to, under the rule it was written to: the filter keeps a key when it is a
free variable of the closure *or* when it is not a plain user lexical, and a
`__mutsu_*` metadata key is never a plain user lexical. So a closure created
anywhere in a scope inherited the type-constraint metadata of every typed lexical
in that scope, including the ones its body cannot possibly name.

That is the third of the three linear-in-import-size terms
[#7565](https://github.com/tokuhirom/mutsu/issues/7565) decomposed. After the
first two were fixed (#7656's END-phaser refresh, #7707's per-chunk reflective
latch and `__mutsu_callable_id::` filter), 11 of the ~35 entries a closure still
captured after a bare `use Test` were these keys, and they scale with the
importing scope exactly like the routine-registration markers #7707 removed.

## The rule

`__mutsu_type::<name>` is *shadow metadata*: it records what `<name>` is
constrained to, and nothing in the language can observe it except through a read
or a write of `<name>` itself. So it belongs in a capture on exactly the same
terms as its subject does — which is the same reasoning
`CompiledCode::is_callee_local_sym` already applies to these keys on the
scoped-overlay return merge. The filter now unwraps the key and decides it by
running the rest of the predicate on the subject.

Deciding it *that* way, rather than by the tempting shortcut "keep it only for a
free variable", is what keeps a **typed dynamic** correct. `my Int $*dyn` is
captured because its env key is a system name, never because it is a free
variable; a free-variable test would have dropped its constraint and let
`$*dyn = 'str'` through inside any closure. Running the real predicate on the
subject handles dynamics, `self`, match captures and the rest for free, because
those are precisely the names the predicate already keeps.

A side effect worth noting: the metadata of a name the closure *shadows* with its
own declaration is now dropped along with the shadowed name, instead of being
inherited by a frame that has no such variable. That is the same cross-frame
constraint leak `set_var_type_constraint_routine_scoped` was scoped to prevent
(`news/2026-09/type-constraint-global-side-table-retired.md`); the capture was
still carrying one case of it.

## The unwrapping is memoized

Reversing `__mutsu_type::<name>` to `<name>` meant resolving the symbol,
re-scanning the prefix and re-interning the suffix — a string hash — every time.
`Symbol::type_meta_subject` memoizes it in the chunked, lock-free table
`FLAG_TABLE` established for `Symbol::flags`
([#7856](https://github.com/tokuhirom/mutsu/issues/7856)): the mapping is a pure
function of an immutable string and symbol ids are append-only, so an entry is
valid for the life of the process and a racing writer stores the identical value.
A thread-local `RefCell<FxHashMap>` was measured first and cost ~41 Ir per hit
against one 32-bit load. Only ids that are actually metadata keys reach the
store, so a program with no typed lexical allocates no chunk.

`CompiledCode::is_callee_local_sym` — which did the same unwrapping by hand, per
metadata key, per named call — now shares it.

## Measured

Instruction counts, not wall clock, per the protocol #7707 established: warm
runs (the first run of any variant writes the precompilation cache and costs
~300M extra instructions), `MUTSU_JIT=off MUTSU_GC=off`, release, slope between
6 000 and 14 000 iterations of the ticket's loop. Callgrind reproduced a repeated
baseline run to within 95 instructions on a ~480M-instruction run, so the slopes
below are good to well under 1 Ir per iteration.

| | instructions per iteration |
| --- | --- |
| the loop with no `use Test` (the floor) | 77 738 → 77 917 |
| `+ use Test`, before | 106 616 |
| `+ use Test`, after | **100 333** |

The import tax is 28 878 → 22 416 per iteration (**-22%**); the loop as a whole
is **-5.9%**. In the callgrind profile of the `use Test` variant the mechanism is
visible directly: `Env::insert_sym`, which is the per-closure-call capture merge,
falls 48.0M → 38.3M (-20%).

The floor moves by +179 instructions per iteration (+0.23%), and that is
optimizer drift rather than added work: an experiment that removed the new branch
from the untyped path *entirely* — a `const` generic on
`env_type_constraint_seen`, so the metadata arm is not even compiled for a
program that has never declared a typed lexical — still measured +180. Two
further shapes of the same logic (the predicate extracted to an `#[inline]`
function; the two "drop this key" flag tests fused into one mask) measured
between +0.1% and +0.7% *worse* on the `use Test` path, which is the sensitivity
band to keep in mind before rearranging this filter for readability.

## Pinned

`t/routines/closure/closure-capture-type-constraint.t` — 12 assertions over the shapes
where the subject *is* captured and the constraint must still bite: a typed
scalar, a typed dynamic, a typed array's element check, a typed hash's value
check, a subject reached only through a nested closure, an escaping closure over
a routine-local typed lexical, and a typed `is copy` parameter; plus the two
shapes where a closure-local redeclaration must *not* inherit an outer or
creating-routine constraint. All twelve agree with `raku`.
