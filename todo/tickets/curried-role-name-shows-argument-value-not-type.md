# A curried parametric role's `.^name` shows the argument VALUE, not its type

Rakudo names a curried parametric role after the *types* of its arguments;
mutsu stringifies the argument values instead.

```raku
role R[Str:D $n] { }
say R["x"].^name;
# raku:  R[Str]
# mutsu: R[x]
```

It propagates into anything that embeds the curried name, e.g. the
`Language/mop.rakudoc` parameterization example, whose `^parameterize` builds
its new name from `roles.map(*.^name)`:

```raku
class Foo {
    method ^parameterize(::?CLASS:U $this is raw, +roles) {
        my Str:D $name   = self.name: $this;
        my Mu    $mixin := $this.^mixin: |roles;
        $mixin.^set_name: [~] $name, '[', roles.map(*.^name).join(','), ']';
        $mixin
    }
}
role R[Str:D $n] { }
say Foo[R['x']].^name;
# raku:  Foo[R[Str]]
# mutsu: Foo[R[x]]
```

## Where it lives

`Value::parametric_role(name, type_args)` is built in
`src/vm/vm_var_index_ops.rs` (the role-parameterization arm of
`exec_index_op_with_positional`), and the `.^name` / `.raku` rendering of a
parametric role is in `src/builtins/methods_0arg/raku_repr.rs` and
`src/builtins/methods_0arg/dispatch_core_repr.rs`. The renderer appears to
stringify each stored type argument; for a non-type argument (a `Str`, an
`Int`) it should render that argument's *type* name instead — Rakudo's curried
role name is built from the role's signature/arguments' `.WHAT`.

Care is needed for the cases that legitimately show a value: a role
parameterized by a type object (`R[Int]`) must still render `R[Int]`, which is
already what falls out of "render the argument's type" only when the argument
IS a type object. Check `.raku` too, and whether any existing whitelisted roast
test pins the current (wrong) spelling before changing it.

Found while fixing
`news/2026-08/grammar-metaclass-parameterize-stack-overflow.md`; it is
independent of that crash and was left out of that PR.
