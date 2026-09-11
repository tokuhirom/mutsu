# A module's own file-scope symbol now beats the caller's same-named lexical

A module's routine read the wrong scope's symbol whenever the program that
loaded it happened to have a lexical of the same name:

```raku
# lib/EKUDefs2.rakumod
unit module EKUDefs2;
our Str enum U2 is export(:U2) « :zz<ZZ> »;

# lib/EKU4.rakumod
unit class EKU4;
use EKUDefs2 :U2;
method c() { zz }      # reads the imported key by its BARE spelling

# t.raku
my $zz = 5;            # <-- unrelated caller lexical, same name
use EKU4;
say EKU4.c.Str;
```

`raku` prints `ZZ`. mutsu printed `Use of uninitialized value of type Any`.
Delete the `my $zz = 5;` line and mutsu printed `ZZ`, so the caller's lexical
was the entire difference. The same happened for an imported sigil-less
`constant` and for a module's own file-scope `constant`, so it was never
specific to enums.

## Root cause

A module body executes in the env of whatever frame loaded it, and that binding
is undone when the load ends. The module's own routines are therefore *not*
served from the live `env` — they read `module_scope_lexicals` /
`module_imported_lexical_names`, or, for a unit class's own `constant`, the
package-qualified `our` store (`EKU4::mss-own`).

`exec_get_bare_word_op` consulted the first of those as its **last** resort, and
the package-qualified store only through `package_chain_var_fallback`, which
anchors on `current_package` — GLOBAL while a method body runs, so it declined
for exactly the methods that needed it. Both sat *below* the generic
`env[name]` probe, so any same-named entry the caller left behind won.

Two different caller entries won, at different times. On the first call the
module saw the `Nil` / `Package("Any")` decl-seed placeholder that a mainline
`my $x` leaves behind, and the bareword read as `Any`. On the second call of the
same routine the caller's *real* value had reached the module's env, and the
bareword read as `5`. Both are wrong, and the second is why a placeholder-only
rule would not have been enough:

```
$ mutsu -I lib -e 'my $zz = 5; use EKU4; say EKU4.c.raku; say EKU4.c.raku;'
Any
5                     # raku: U2::zz both times
```

## Fix

`running_module_bareword`, consulted immediately above the generic `env[name]`
probe and gated — like the variable-read twin in `get_env_with_main_alias_sym` —
on the running routine belonging to a unit compunit. It splits the three sources
by whether a lexical could ever collide with them:

- `module_imported_lexical` and the package-qualified `our` store answer
  **unconditionally**. Neither namespace is one a lexical of the loading scope
  can occupy — the second because its keys carry a `::` — and
  `module_imported_lexical`'s existing contract is already "an imported alias
  must beat the caller's same-named env entry".
- `module_scope_lexicals` is keyed by bare name and *can* collide with a
  captured local of the module's own routine, which must keep the name. It
  answers only when `env` holds nothing, or nothing but a decl-seed placeholder.

The `our`-store walk (`running_package_our_var`) anchors on
`running_package_candidates` — the running-frame package list extracted from
`lookup_in_running_package`, so both now agree on what "the running package" is
instead of one of them re-deriving it from the GLOBAL `current_package`.

Placement matters as much as the rule. The probe sits below every type route
(`resolve_suppressed_type`, the type-object branch, `has_type`,
`resolve_type_in_current_package`) and below the #7914 enum-key namespace probe,
so exactly one thing is reordered: the env-alias fallback that was serving the
wrong scope's symbol.

## Still open

The **mainline** half of the collision is untouched and deliberately so: in

```raku
my $cc = 'CALLER';
use Mix1;      # exports `constant cc = 'CCVAL'`
say cc;        # raku: CCVAL, mutsu: CALLER
```

both symbols genuinely share the one `env` key, so no precedence rule can
separate them — that needs a storage namespace of the kind
[#7914](https://github.com/tokuhirom/mutsu/issues/7914) gave enum keys, and
#7914's own "Still shared" note already records it. The frame guard means this
change cannot reach that case; verified under a debugger rather than inferred.

Pinned by `t/modules/module-scope-symbol-beats-caller-lexical.t` (10 assertions
against the fixtures `t/lib/ModuleScopeSymbolDefs.rakumod` and
`t/lib/ModuleScopeSymbolUser.rakumod`, output verified identical against the
rakudo oracle), which covers all three symbol kinds on both the first and the
second call, the captured-local case that must *not* change, and the caller's
own lexicals still reading as themselves.

Closes #7960.
