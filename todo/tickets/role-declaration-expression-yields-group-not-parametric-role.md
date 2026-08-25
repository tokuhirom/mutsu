# A `role` declaration used as an expression evaluates to the role *group*, not the individual parametric role

Originally filed as "a parenthesized anonymous `role` declaration fails to parse as an
expression term" (found by the doc-diff harness,
`Type/Metamodel/ParametricRoleHOW.rakudoc:29`). **Re-measured on `main` @ `17139dd55`
(2026-08-25): the parse failure is gone** — `(role Zape2 {}).HOW.say` compiles and runs. What
remains is a different, narrower divergence in the *value* the declaration expression produces,
so the ticket is rescoped rather than closed.

## Repro

```raku
(role Zape[::T] {}).HOW.say;
(role Zape2 {}).HOW.say;
```

| expression | raku | mutsu |
| --- | --- | --- |
| `(role Zape[::T] {}).HOW` | `Perl6::Metamodel::ParametricRoleHOW.new` | `Perl6::Metamodel::ParametricRoleGroupHOW.new` |
| `(role Zape2 {}).HOW` | `Perl6::Metamodel::ParametricRoleHOW.new` | `Perl6::Metamodel::ParametricRoleGroupHOW.new` |

## Root cause

In Rakudo these are two distinct meta-objects. `role R { }` installs a **role group**
(`ParametricRoleGroupHOW`) under the name `R`, which dispatches across every same-named
candidate; the group holds one or more **individual parametric roles**
(`ParametricRoleHOW`), one per declaration. The *value of the declaration expression itself*
is the individual role that was just declared — not the group it was added to.

mutsu conflates the two: the declaration expression yields the same object the installed name
resolves to. The control cases confirm the split is exactly there and nowhere else:

```
$ mutsu -e 'role R {}; say R.HOW;'            # Perl6::Metamodel::ParametricRoleGroupHOW.new  -- matches raku
$ mutsu -e '(class Foo {}).HOW.say;'          # Perl6::Metamodel::ClassHOW.new                -- matches raku
```

Looking up the installed *name* already gives the group correctly, and the equivalent `class`
declaration-as-expression already gives the right meta-object; only the role declaration
expression's own value is wrong. So the fix is to make role declaration in expression position
return the individual `ParametricRoleHOW`-backed role it minted, while the name binding keeps
pointing at the group.

## Why it matters beyond `.HOW`

`ParametricRoleHOW` vs `ParametricRoleGroupHOW` is not only a display difference — they answer
`.^candidates`, `.^parameterize`, and role-composition introspection differently. Anything that
reaches a role through its declaration expression (rather than through its installed name)
therefore sees the wrong meta-protocol.

This is likely related to
[metamodel-parametricrolehow-new-type-wrong-how.md](metamodel-parametricrolehow-new-type-wrong-how.md)
and [role-instance-how-wrong-metaclass.md](role-instance-how-wrong-metaclass.md), which are
about the same two HOWs being confused at other entry points; a single underlying
"individual role vs role group" distinction may fix all three.

## Affected files (starting point)

- Wherever a `role` declaration is registered and its declaration-expression value chosen —
  `src/runtime/types/roles.rs` and the compiler/VM path that gives a package declaration in
  expression position its value. Compare against how `class` already does it, since the class
  path produces the correct meta-object.
