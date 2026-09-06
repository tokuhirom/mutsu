# A module's scalar-held container no longer collides with the caller's `my $x`

A `unit module`'s file-scope `my $a = [...]` used to share its array with a
consumer's same-named `my $a`: the module's own `$a.push($v)` extended the
CONSUMER's array and left the module's untouched. The same held for `our $a` and
`state $a`, for every array mutator (`push`/`append`/`unshift`/`prepend`/`pop`/
`shift`), and — through a second, independent mechanism — for a method of a class
declared inside the module, which could permanently *rebind* the consumer's
variable to the module's container merely by reading it.

Filed as `todo/tickets/module-scalar-held-array-collides-with-caller-my.md`
during the ADR-0039 slice-2 acceptance work, where the two `$anon` rows had to be
dropped from `t/container-lexical-declarator-matrix.t`. Both are now fixed and
both rows are back.

## Why the reduction looked clean

The ticket's headline finding was that the divergence is context-sensitive: the
two-line repro passes on its own and only fails once the consumer file also
declares other mainline lexicals and named subs. A mechanical prefix bisection
named the trigger exactly, and it is much smaller than the ticket guessed — it is
not *which* names the mainline capture holds, it is simply **the presence of any
mainline named `sub` in the consumer at all**. `sub noop() { 1 }`, with no free
variables and no relation to `$anon`, is enough; a bare block is not. A `sub`
declaration makes the mainline mirror its `my $anon` into `env` (the hoisted
`RegisterDecl` is the only bytecode difference between the passing and failing
programs), and the bug is a read of that `env` entry.

The ticket's suggested root cause — `unit_lexical_container_cell` probing
`unit_lexicals[MAINLINE_UNIT_KEY]` before `unit_lexical_slot` — was wrong. A
`rust-gdb` breakpoint on the mainline-capture insert in `exec_register_sub_op`
never fired, and `MUTSU_VM_STATS` reported `adr0024-mainline-lexicals: boxes=0`
for the failing program: the mainline bucket was empty the whole time.

## Root cause 1: the sigil-less lane never got the write chokepoint

`$a.push(...)` on a scalar-held array lands in the "sigilless array bindings"
block of `runtime/methods_mut_dispatch.rs` (the `callmethodmutwithvalues:
sigilless-push-append` intercept). Every slot access there read and wrote
`self.env` directly under the bare, sigil-less name (`anon`, not `$anon`).

That is precisely the key ADR-0039 slice 1 established belongs to *whatever scope
loaded the compunit*: a module's own file-scope lexical lives in `unit_lexicals`,
and the loading scope's same-named `my` is restored into `env` once the module's
mainline finishes. The `@`/`%` mutator arms were routed through
`env_root_descended_mut` (which consults `unit_lexicals` first) by that slice;
the sigil-less arms never were. With `slot_is_array` measured as `false` in the
passing program and `true` in the failing one, the two programs took different
branches of the same `if` — the failing one writing straight into the consumer's
array.

All six mutators plus the `array_flag` probe now go through
`env_root_descended_mut`, exactly like their `@`/`%` twins.

`our $a` needed one thing more. `env_root_descended_mut` consulted
`our_package_container_mut`, which gates on an `@`/`%` sigil, so a bare `our`
SCALAR name fell straight through to `env`. The read side
(`get_env_with_main_alias`) already had the sigil-less twin
(`our_package_scalar`), so the write chokepoint simply gained the matching
`our_package_scalar_mut` arm in the same position. The two chokepoints now mirror
each other exactly.

`state $a` needed nothing: `collect_unit_lexical_names` already collects
`state` alongside `my`, so fixing the chokepoint fixed it.

## Root cause 2: a method's captured env leaked back into the caller

Found by varying the ticket's shape rather than stopping at its repro. It is a
strictly worse bug, it is *not* the same root cause, and it hits `@`/`%`
containers too — the shapes the declarator matrix claims are correct:

```raku
# module M:  my @arr = <a b>;  class C is export { method apeek() { @arr.join(",") } }
my @arr = <x y z>;
C.new.apeek();          # a READ, nothing else
say @arr.join(",");     # raku: x,y,z   mutsu (before): a,b
@arr.push("Q");         # ... and the push then landed in the module's array
```

A method of a class declared in a module carries the module mainline as its
`method_def.captured_env`. When that capture is *authoritative*
(`__mutsu_declared_method_capture`), `call_compiled_method_fast` overwrites the
caller's same-named env entry with the module's value on entry — correct, that is
what makes the method see its own defining scope. On exit, `merge_method_env`
then saw that entry in the callee overlay, found the caller had the same key, and
merged it back, reporting it in `changed_caller_locals`; the Slice F write-through
then pushed it into the caller's compiled-local slot. Measured with a gdb
breakpoint on the writeback list, which named exactly one symbol per leaking call.

`merge_method_env` now skips a key it received from `captured_env` whose callee
value is still *identical* to the captured one (`cheaply_unchanged`, the same
O(1) identity test the merge already uses). A key the method actually wrote still
merges, so the "class declared in a routine mutates its captured outer lexical"
propagation is untouched; a key the method merely read is recognised as frame
setup rather than a caller-visible write.

## Pins

- `t/module-scalar-held-container-lexical.t` (24 assertions, byte-identical under
  `raku`): the full mutator matrix for `my $`/`our $`/`state $`-held arrays, the
  scalar-held hash, and both captured-env method rows. Its fixture is
  `t/lib/ScalarHeldLexical.rakumod`. The mainline `sub trigger()` in it is
  load-bearing and says so — without a mainline named sub the consumer never
  mirrors its lexical into `env` and root cause 1 is invisible.
- `t/container-lexical-declarator-matrix.t`: the two `$anon` rows are restored
  (plan 41 -> 43).
