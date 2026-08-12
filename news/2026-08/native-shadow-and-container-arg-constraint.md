# User methods shadow inherited native methods; container args stop inheriting stale name constraints (46_eol_si 718/718)

Two general dispatch bugs found via Text::CSV's `t/46_eol_si.t`, which now
passes 718/718 (was: 61 failures and an early abort at 373).

## 1. Inherited native methods beat the subclass's own methods

`dispatch_instance_and_fallback`'s native fallback fired whenever
`is_native_method` found the method anywhere in the MRO — so for a user
subclass of a builtin (`Text::IO::String is IO::Handle` with its own
`method Str`), `~$fh` stringified through the native IO::Handle `Str`
(`"IO::Handle()"`) instead of the user method. Text::IO::String's close-time
writeback (`$!str = ~ self`) therefore wrote garbage into the test's string
buffer, and every second-pass `getline` read `["IO::Handle()"]`. The
fallback is now gated on `!has_user_method` (the user-method block further
down dispatches it). Explicit `.Str` already worked — only the nested
dispatch path (`~` → `Stringy` → `Str`) hit the native arm first.
Pin: `t/native-method-user-shadow.t`.

## 2. Binding a container argument consulted the scope-blind name store

`bind_function_args_values` derived a parameter's element-type constraint
from `var_type_constraint(source_name)` — the NAME of the caller's argument
variable in the global, scope-blind store. A module method's own
`my CSV::Field @f` declaration leaves a global `"@f"` entry behind, so a
caller's UNTYPED `@f` passed to ANY later method call got retyped: tagged
`Array[CSV::Field]`, rendering `.raku` as `Array[CSV::Field].new(...)` (the
test compared it against plain `[...]` — 59 failures). Container arguments
now read the element/key type from the VALUE's embedded metadata (tagged at
a typed declaration's assignment), which is immune to name collisions;
scalar sources keep the name-store lookup. Legitimate propagation
(`my Int @a; f(@a)` → `.of` is `Int`) still works — verified along with the
full S09-typed-arrays roast set.
Pin: `t/container-arg-no-stale-name-constraint.t`.

Also hardened in passing: `SetVarType` (declaration position) no longer
tags a pre-existing same-named env value with container metadata — at
declaration time that value belongs to an outer scope or a previous loop
iteration (`set_var_type_constraint_decl`).

Both are instances of the scope-blind name-keyed store disease tracked in
`todo/deep/bare-name-type-constraint-store-is-scope-blind.md`.

Text::CSV suite after this: everything green except `66_formula` (blocked
on that deep ticket), `90_csv` 159/507/508 (raku-parity + kh ticket), and
`99_meta` (fails under rakudo too).
