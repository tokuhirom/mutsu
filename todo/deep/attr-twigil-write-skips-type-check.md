# `$!attr = v` inside a method skips the attribute's declared type check

Found 2026-07-25 in HTTP::Request. Moved out of PLAN.md §8.21 when discovered
findings became per-file `todo/` entries.

## Repro

```raku
class R { has Int $.n is rw; method f($v) { $!n = $v } }
my $r = R.new(n => 1);
$r.f("nope");   # raku: X::TypeCheck::Assignment; mutsu: silently stores "nope"
$r.n = "nope";  # both throw — only the in-method twigil forms are affected
```

`$.attr = v` (the public rw accessor written with the `.` twigil) has the same
hole; `self.attr = v` correctly throws. A `subset` constraint behaves the same as
a plain type, so HTTP::Request's
`subset RequestMethod of Str where any(<GET POST …>)` accepted
`set-method('TEST')`.

## Root cause

The rw-accessor lvalue path checks `get_attr_type_constraint`
(`methods_mut_method_lvalue.rs`), but a `$!attr` / `$.attr` write is compiled as
an ordinary name assignment: it lands in a local slot / env with a separate
mirror into the instance's attribute cell (`write_self_attr_cell` and friends).
None of those write paths consults the attribute's declared type.

## Affected files

- `src/runtime/methods_mut_method_lvalue.rs` — the accessor path that *does*
  check.
- `src/vm/vm_var_assign_computed_attr.rs` — `write_self_attr_cell` /
  `write_attr_cell_by_key`, the shared write tail.
- The write ops that mirror into it: `vm_var_assign_set_local.rs`,
  `vm_var_assign_typed.rs`, `vm_var_assign_post_incdec.rs`,
  `vm_smartmatch_ops.rs`, `vm_misc_codevar.rs`.

## Why it is large

There are several write paths (`SetLocal`, `SetGlobal`, the name-based assign,
post-inc/dec, the smartmatch topic writeback); the shared tail
`write_attr_cell_by_key` takes `&self` and returns `()`, so it cannot raise; and
the check has to run *before* the slot write, or a caught failure leaves the slot
holding the rejected value. The existing `var_type_constraints` map is name-keyed
and would conflate `!n` across classes, so it cannot be reused as-is. This needs
a per-class attribute-constraint lookup threaded into a single pre-store choke
point.

## Impact

HTTP::UserAgent `t/040-request` subtest 18 ("rejects wrong method"); more
broadly, every typed attribute in every class is unenforced from inside its own
methods.
