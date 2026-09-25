# A named sub's rebind of a bound mainline lexical no longer overwrites its source

`my @a = 1, 2; my @b := @a; sub f { @b := [9] }; f()` left `@a` as `[9]`: the
rebind inside the named sub replaced the contents of the array `@b` was bound
to, instead of re-pointing `@b`. Scalars fared worse under a shadowing caller
block: `my $x = 1; my $d := $x; sub g { $d := 0 }; { my $d = 9; g() }` turned
`$x` into `0` (#9416).

A mainline sub reaches its free variables through the compunit lexical store
(ADR-0024), whose entry for `@b` was the plain cell `@b` shares with `@a` after
the bind. `SetGlobal` stored the rebind's new value *through* that cell. Simply
replacing the store entry would have disconnected the mainline frame's own
slot, which keeps the old cell and so would never see the rebind.

The fix reuses the binding-cell mechanism a closure's capture already has
(#9307). When `RegisterSub` fills the store for a name the sub (or a closure
inside it) rebinds with `:=`, it wraps the name's container in a binding cell,
a cell whose content is the variable's container. The mainline slot and the
store now share that cell. A rebind to a value reaching `SetGlobal` seats the
new container inside it (`unit_scope_lexical_rebind`), so the mainline sees the
new binding while `@a` keeps its own container. A rebind to another variable
already replaced the binding cell's content, and now leaves the source alone
too.

A rebind-only free variable (`sub k { $f := 5 }` as a statement) is now also
captured at all: it appears in neither the sub's read set nor its write set, so
it used to fall back to the flat, name-keyed `env`.

An uppercase scalar name (`$D`) is still not captured by the store at all, so
the write lands on a shadowing caller's `my`. That is filed separately as
#9459.

Pin: `t/vm/binding/bind-rebind-from-named-sub-leaves-source.t`.
