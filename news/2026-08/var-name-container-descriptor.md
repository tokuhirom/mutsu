# Container-descriptor .VAR.name for @/% parameters (Text::CSV runtime sweep round 7)

Rakudo's `.VAR.name` on a `@`/`%` parameter reports the *container
descriptor*'s name, not the parameter's syntactic name: an unsupplied
optional (`sub t(:@kh) { }; t()`), a literal argument, a slurpy, and an
`is copy` param all bind a fresh anonymous container named **"element"**,
while a param aliasing a caller's named container reports that caller's
sigiled name (`t(kh => @x)` → `"@x"`). mutsu always answered the param's own
name (`"@kh"`).

This mattered because Text::CSV's `method CSV` contains a rakudo#2483
workaround that gates its entire output-mode defaulting on exactly this:

    if (@kh.VAR.name ne "element") { # @kh = "@kh" or [] = "element"
        $out     //= Hash if $out === Any;
        $headers //= "auto";
        }

With mutsu's always-`"@kh"` answer the guard was always true, so every
`csv(in => ...)` call silently switched to AoH output mode, shifted the
header row into column names, and (for AoH input) emitted empty hashes —
the bulk of 90_csv.t's 22 failures.

Mechanism (three pieces):

- **A fresh unsupplied-default container is tagged in its own data**:
  `missing_optional_param_value` now builds the `@`/`%` seed via
  `Value::element_descriptor_array()/_hash()`, which set a new
  `descriptor_name: Option<Box<str>>` field on `ArrayData`/`HashData`
  ("element"). The tag travels with the value, so both the light and slow
  bind paths are covered at one chokepoint.
- **The slow binder records caller sources**: positional by-ref container
  params already registered `(param, source)` in `rw_bindings`; supplied
  *named* container params now record theirs in a parallel list, and a pass
  at the end of `bind_function_args_values` writes
  `__mutsu_var_source_name::` env metadata (caller's name, or "element" for
  literal/copy/slurpy bindings).
- **The `.VAR` reflector validates its cache**: the built Variable-meta
  instance is cached per name (`var_meta_value`), which froze the FIRST
  call's answer for every later call of the same sub (call 1 unsupplied →
  "element" forever, even once supplied). The reflector now recomputes the
  descriptor name (value tag → env metadata → syntactic name) and rebuilds
  the meta instance when the cached one's `name` disagrees.

Also fixed on the way: a supplied `@`/`%` named param in the light bind path
now always refreshes the env entry alongside its slot (`bind_value` with
forced `needs_env`) — a previous unsupplied call's seed otherwise stayed
visible to env-reading ops in the body while the slot held the real
argument.

Deviation kept deliberately: for a supplied-from-variable *named* param the
light path reports the param's own name (`"@kh"`) rather than rakudo's
caller name (`"@x"`) — the argument value carries no source there. Both
satisfy the `ne "element"` contract; the pin asserts exactly that shape.

90_csv.t: 22 failures → 3 (only the Channel in-format and its follow-on
abort remain). Pin: `t/var-name-container-descriptor.t` (14 assertions,
raku-verified).
