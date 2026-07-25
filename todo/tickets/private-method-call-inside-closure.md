# A private method called from inside a closure in the same class loses permission

`self!priv(...)` works directly in a method, but the same call inside a `sub` or
block defined in that method is rejected as an out-of-package private call.

## Repro

```raku
class C {
    method !secret(*@args) { 'S:' ~ @args.join(',') }
    method direct()   { self!secret(1, 2) }
    method via-sub()  { sub (*@args) { self!secret(|@args) } }
    method via-block() { { self!secret(9) } }
}
my $c = C.new;
say $c.direct;          # both: S:1,2
say $c.via-sub()(3, 4); # raku: S:3,4
say $c.via-block()();   # raku: S:9
```

```
raku:   S:1,2 / S:3,4 / S:9
mutsu:  S:1,2, then
        Calling private method 'secret' must be fully qualified with the package
        containing that private method.
```

The permission check evidently keys off the *currently executing* routine's
package. A closure created inside a method of `C` is still lexically inside `C`,
so `self!secret` must remain legal — Rakudo decides this at compile time from the
lexical scope of the call site, not from the frame that happens to be running.

## Impact

This one error takes out **22 of the 23** upstream test files of
`Template::Jinja2` (22/23 under raku, 0/23 under mutsu) — the module's
`LoopContext.get` returns `sub (*@args) { self!cycle(|@args) }`
(`lib/Template/Jinja2/Renderer.rakumod:19`), so `Renderer.rakumod` fails at load
time and every test that `use`s it dies before its first assertion.

It is the single biggest lever in the template-battery survey; see
`todo/deep/template-engines-blocked-on-mutsu.md` and
`docs/batteries/templates.md`.

## Affected area

The private-method dispatch permission check — wherever the "must be fully
qualified" error is raised. It needs the *lexical* package of the call site
(which the compiler knows) rather than the runtime routine's owner.
