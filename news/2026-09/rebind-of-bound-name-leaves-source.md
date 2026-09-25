# Rebinding a bound name no longer writes into the variable it was bound to

`my $z := $y; $z := 5` must re-point `$z` at a new value and leave `$y` alone.
mutsu wrote the 5 into `$y` instead. The same happened when a callee rebound a
dynamic variable (`my $*D := $x; sub f { $*D := 0 }` turned `$x` into 0). That
is exactly what rakudo's REPL protocol does to `$*CTXSAVE`, so
`t/tooling/nqp-getcomp-repl-context.t` had to assign it instead of binding
it (#9357).

There were three separate write paths, and each still treated the rebind as a
store into the old binding:

- **Forward alias.** `my $z := $y` records the alias `z -> y`. A later rebind
  only dropped aliases pointing *to* the rebound name (the #9207 direction).
  So the alias walk after the store carried the new value on into `$y`. A
  rebind now drops the name's own forward alias and the bind pairs where it
  is the target. By-name rebinds (`SetGlobal`) overwrite the alias key with
  `Nil` rather than removing it. A removal made in a callee's env overlay is
  not merged back on return, and the stale alias would otherwise send the
  declaring frame's next `$D = v` into the old source.
- **By-name env write.** A closure's `$D := 0` reached `SetGlobal`, whose
  env write stores *through* the `ContainerRef` cell the name holds. After
  `my $D := $x` that cell is the one `$x` shares. A rebind to a value now
  replaces the entry (`set_env_with_main_alias_fresh_binding`). When the
  name has a #9307 binding cell, the existing reseat puts the new value into
  it, so sibling closures still see the rebind.
- **Whole-container rebind.** `%b := {...}` carries the bind mark as well as
  the rebind mark, so `SetLocal`'s "detach the env cell on rebind" guard
  (`!is_bind`) skipped it. The env write then replaced `%a`'s contents. The
  guard now admits `@`/`%` rebinds.

The regression test is `t/vm/binding/bind-rebind-alias-leaves-source.t`. The
REPL test now binds `$*CTXSAVE` the way rakudo does and checks that `$repl`
survives.

This fix does not cover a named sub rebinding a captured mainline `@`/`%` that
was itself bound to another array (`my @b := @a; sub f { @b := [9] }`). That
store goes through the compunit lexical store's shared cell, and the name has
no binding cell to reseat, so `@a` is still overwritten. That is tracked as #9416.
