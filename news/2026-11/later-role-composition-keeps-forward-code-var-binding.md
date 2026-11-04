# A later `class X does R` no longer breaks a closure's call of a later-bound `my &f :=`

```raku
role R { }
class Id1 does R { }
my &a1 := -> Id1 $a { b1 $a }
my &b1 := -> Id1 $a { say "b1 ok" }
a1 Id1.new;
class Id2 does R { }   # mutsu: Unknown function: Callable   raku: b1 ok
```

A class that composes a role after a run-time statement is a BEGIN-time effect,
so the unit's BEGIN prologue (ADR-0134) is non-empty and splits the earlier
`my &f := ...` declarations: the declaration-only half (`static_bind_decl`)
moves into the prologue, which declared `&b1` before the closure `a1` of the
remainder was compiled. The closure then treated `&b1` as an outer code
variable and captured it eagerly: a `Nil` at creation, so the call fell back to
the by-name lookup and read the declaration's `Callable` placeholder. Without
the later class the unit has no prologue and `&b1` is still a forward reference
when `a1` compiles, which `compute_free_vars` deliberately leaves out of the
closure's free variables.

The split half now carries `VarMarks::static_bind_half`. Once the compile has
passed `Stmt::BeginPrologueEnd`, the compiler leaves a name whose binding
declaration has not compiled yet out of a closure's outer code variables
(`Compiler::static_half_amp`); the prologue's own routines, which are the reason
for the static half (#11263), still capture it.

Found on IdClass 0.0.3 (`t/06-role.rakutest`); pinned by
`t/oo/role/later-role-composition-keeps-forward-code-var-binding.t`.
