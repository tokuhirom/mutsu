# `Metamodel::Documenting`: `.HOW.set_why` after `.HOW.compose`

`Type/Metamodel/Documenting.rakudoc`'s worked example builds a type by hand and then
attaches documentation to its metaclass:

```raku
BEGIN {
    our Mu constant Documented = Metamodel::ClassHOW.new_type: :name<Documented>;
    Documented.HOW.compose: Documented;
    Documented.HOW.set_why: do {
        my Pod::Block::Declarator:D $pod .= new;
        $pod._add_leading:  "Documented is an example class ...";
        $pod._add_trailing: "Take a look at my WHY!";
        $pod
    };
}
say Documented.HOW.WHY;
```

mutsu died with `Cannot modify an immutable 'Documented' type object`. The ticket's
hypothesis — that mutsu's post-compose lock was over-broad and wrongly covered
metadata-only HOW mutators — turned out to be wrong about *where* the error came from:
`set_why` did not exist at all, and the "immutable" error was thrown three statements
earlier, by the `our Mu constant Documented = ...` binding. Four separate gaps had to
close.

## 1. `constant` is a declaration, not a modification

`src/vm/vm_exec_dispatch.rs` rejects a bareword store when the name is already a
registered class holding a `Package` value — the guard that makes `Foo = 5` fail with
Rakudo's "Cannot modify an immutable 'Foo' type object". But
`Metamodel::ClassHOW.new_type(:name<Documented>)` *registers a class literally named
`Documented`* while evaluating the right-hand side, so by the time the binding ran the
guard saw its own subject and refused.

The guard now exempts a `constant` declaration (`raw_mode`). A declaration binds the
name rather than modifying what the name currently means, which is why Rakudo also
accepts `constant Int = 5` (it prints `5`) while still rejecting `Foo = 5`. This also
closes the headline symptom of the older deep ticket
`todo/deep/direct-metamodel-classhow-new-type-immutable-error.md`, whose remaining,
narrower gap (calling a `.^add_method`-installed method on a `new_type`-minted type
object is a silent no-op) is recorded there.

## 2. A definedness smiley is not part of the `.=` invocant

`my Pod::Block::Declarator:D $pod .= new` desugared to a bareword lookup for a package
literally named `Pod::Block::Declarator:D`. See
`news/2026-08/dot-assign-target-keeps-definedness-smiley.md`, fixed in the same change.

## 3. `Pod::Block::Declarator._add_leading` / `._add_trailing`

These are how Rakudo accumulates `#|` / `#=` declarator comments, and the only public
way to build a declarator pod block by hand. Implemented as a native 1-arg method in
`src/builtins/methods_narg/dispatch_1arg.rs`: each call space-joins onto the block's
`leading`/`trailing` text (the shape `.leading` already reports, and the shape
`make_pod_declarator` already produces for `#|` comments) and recomputes the newline-
joined `contents` that `.Str`/`.gist` render. `InstanceAttrs` is interior-mutable, so
the append is visible through every alias of the block — which is what makes the
documented `my $pod .= new; $pod._add_leading(...); $pod` idiom work.

Rakudo hands back the raw `@!leading` array; mutsu returns the accumulated string,
since it stores the joined text. Every documented idiom uses the call in sink context.

## 4. `set_why` / `WHY` on the metaclass

New arms in `src/runtime/methods_classhow_dispatch.rs` store the pod under
`type_metadata[type]["__set_why__"]` and read it back. `dispatch_why` consults the same
slot for a `Package` target, so `Documented.WHY` and `Documented.HOW.WHY` agree — as
they do in Rakudo.

One arity subtlety worth recording: `Metamodel::Documenting.set_why($why)` is the only
HOW mutator whose Rakudo signature has **no `$obj` parameter**. `C.^set_why($pod)` is
an arity error in Rakudo ("expected 2 arguments but got 3"); `C.HOW.set_why($pod)` is
the only spelling. `how_dispatch_args` therefore prepends the receiver's own type for
that method specifically, so the dispatcher still sees the uniform
`(invocant, value)` shape every other mutator arm uses.

Pinned by `t/metamodel-introspection.t`, which runs the doc's example verbatim.
