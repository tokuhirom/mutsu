# A named parameter's alias spelling made a pure clone mutate its receiver

`Graph.directed-graph()` returns a new, independent `Graph`. Under mutsu it also
changed the graph it was called on: the original's `edge-count` went from 4 to
8, because `$!directed` on the *receiver* was set to `True` by a method that
never assigns to it.

## Root cause

Attributes live in a shared cell, and a method's exit does one last pass
(`reconcile_attrs` in `src/vm/vm_method_dispatch.rs`) looking for attributes the
body rebound with `:=`. Such a binding shows up as a `ContainerRef` in the
frame's locals or env overlay under the attribute's name, so the pass scans each
attribute name for one — and then guards against the obvious false positives,
because an ordinary parameter or `my` lexical can carry an attribute's bare name
too and is boxed into exactly the same shape the moment it is passed by
reference or lands in a list.

That guard consulted the routine's flat parameter-name list, which is not the
set of lexicals a signature binds. Two spellings bind a name that never appears
in it:

- the named-alias form — `:d(:$directed)` binds `$directed`, while the
  parameter's own name is the external key `d` and the real name sits in its
  sub-signature;
- a destructuring sub-signature — `sub f([$a, $b])` binds `$a` and `$b`.

`Graph`'s `method clone(:d(:$directed) is copy = Whatever)` is the first form.
`$directed` is boxed the moment the body writes `given ($directed, $!directed)`,
and because the flat list only said `d`, the exit pass concluded that
`$directed` was not one of the frame's own parameters, read it as a `:=` binding
of the attribute `$!directed`, and committed it to the receiver's cell. The
`is copy` is incidental; so is the `given`. Any list containing the parameter
does it, and the attribute did not need to be mentioned at all.

The minimal shape:

```raku
class Foo {
    has Bool:D $.directed = False;
    submethod BUILD(Bool:D :directed-edges(:$!directed) = False) { }
    multi method new(Bool:D :d(:directed-edges(:$directed)) = False) { self.bless(:$directed) }
    method clone(:d(:$directed) is copy = Whatever) {
        my $t = ($directed, False);
        return Foo.new(:directed);
    }
}
my $a = Foo.new;
$a.clone(:directed);
say $a.directed;   # rakudo: False    mutsu (before): True
```

## The fix

`reconcile_attrs` now asks the signature itself rather than the flattened name
list: a new `crate::ast::param_defs_declare_lexical` walks `ParamDef`s and their
`sub_signature` / `outer_sub_signature` children, so an alias-bound or
destructured parameter is recognized as a name the frame owns and is never
adopted as an attribute binding.

Pinned by `t/oo/attribute/attr-alias-arg-not-adopted-as-bind.t`, which covers
the alias form, the plain spelling that always worked, and the destructuring
sub-signature case.

Closes #9007.
