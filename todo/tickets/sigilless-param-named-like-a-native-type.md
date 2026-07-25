# A sigilless parameter named like a native type (`\str`) reads the type object in a module sub

Found 2026-07-25 while fixing the builtin-shadow dispatch bug for `String::Rotate`
(`TODO_dist` T-057). With that fixed the dist's exported `sub rotate` is finally
*called*, and this is what it does wrong.

## Repro

`tmp/mlib/MyRot2.rakumod`:

```raku
unit module MyRot2;
sub c_same (Str(Any) \str, Int \ch = 1 --> Str) is export { "[{str}]/[{ch}]" }
sub c_diff (Str(Any) \zzz, Int \ch = 1 --> Str) is export { "[{zzz}]/[{ch}]" }
sub p_same (Str      \str, Int \ch = 1 --> Str) is export { "[{str}]/[{ch}]" }
sub s_same (Str      $str, Int $ch = 1 --> Str) is export { "[$str]/[$ch]" }
```

```raku
use MyRot2;
my $str = 'Rakudo';
say c_same($str, 3);   # raku: [Rakudo]/[3]   mutsu: []/[3]  + "Use of uninitialized value element of type str"
say c_diff($str, 3);   # both: [Rakudo]/[3]
say p_same($str, 3);   # raku: [Rakudo]/[3]   mutsu: []/[3]
say s_same($str, 3);   # both: [Rakudo]/[3]
```

The failing factor is the **parameter's own name**, not the argument's: renaming
`\str` to `\zzz` fixes it, and passing a differently-named variable
(`c_same($other, 3)`) still fails. A sigiled `$str` is fine. The bound value's
gist is `(str)` — the **native `str` type object**.

Declaring the same sub in the main script instead of a module works, so it needs
the module frame (where the routine runs with `current_package` set to the
module).

## Root cause (hypothesis, needs confirming)

A sigilless parameter is stored in env under its bare name (`str`). Inside a
module routine, resolving the bare name `str` in the body appears to reach the
built-in native **type** name before the parameter binding — the parameter does
not shadow the type name in that scope. `\ch` is unaffected because `ch` is not a
type name; `$str` is unaffected because the sigil disambiguates.

Confirm by checking the bare-name read path used for a sigilless param inside a
package-scoped routine, and where builtin type names are injected into a module
frame's env.

## Affected files

- `src/runtime/types/binding_signature.rs` — the `pd.sigilless` binding block
  (bare-name env insert)
- the bare-name read path (`vm_var_ops.rs` / `vm_env_helpers.rs`) and whatever
  gives a builtin type name precedence in a package scope

## Impact

`String::Rotate` `t/01-basic.rakutest`: the 68 `sub rotate` subtests (the 68
`method rotate` ones pass). Its signature is literally
`sub rotate (Str(Any) \str, Int \ch = 1 --> Str)`. Any module sub with a
sigilless parameter named after a native type (`str`, `int`, `num`, `bool`, …)
is affected — an easy name to pick.

Related but distinct: [[the sigilless reread bug]] fixed in
`news/2026-07/sigilless-param-rereads-reset-topic.md` was about the *argument's*
source name; this one is about the *parameter's* name.
