# `Proxy` returned from an `is rw` routine now reaches the caller as a container

`Proxy` is the documented core type whose whole purpose is to be bound to a
name and then read and written through its `FETCH`/`STORE` methods. The
`Type/Proxy.rakudoc` synopsis — an `is rw` sub returning a `Proxy`, bound with
`:=` and then assigned to — did not work in mutsu at all:

```raku
sub double() is rw {
    my $storage = 0;
    Proxy.new(
        FETCH => method ()     { $storage * 2    },
        STORE => method ($new) { $storage = $new },
    )
}
my $doubled := double();
$doubled = 4;
say $doubled;      # raku: 8   mutsu: Cannot assign to an immutable value
```

## Root cause: the wrong half of the decontainerization rule

Raku decontainerizes a routine's return value, so a `Proxy` built inside an
ordinary `sub` reaches the caller already FETCHed. `is raw` and `is rw` both
suppress that: the caller receives the `Proxy` itself and the FETCH happens at
the next *use*. That is what makes the synopsis work — the `:=` binds a
container, so the later `=` has a `STORE` to run.

mutsu's return path tested `is raw` alone (`cf_auto_fetch = !cf.is_raw`), and
the code-object path had the rule inverted outright (`auto_fetch = data.is_rw`,
so an `is rw` routine was the *only* shape that FETCHed). Every `is rw` return
was therefore resolved at the call, the binding held a plain value, and the
assignment died as immutable.

The decision is now stated once per carrier and reads the whole ADR-0067 rule
— `is rw`, `is raw`, **or** an explicit `return-rw`:

- ADR-0067's existing `Interpreter::routine_is_rw_capable` /
  `method_is_rw_capable` wherever a resolved def was already in hand;
- `CompiledFunction::returns_container()` for the compiled path, which carries
  no body AST and so gets a new `uses_return_rw` computed once at declaration
  time (so `sub f() { return-rw Proxy.new(...) }` binds a container now too);
- `SubData::returns_container()` for the code-object path.

`SubData` answers *container* for anything it cannot prove decontainerizes: a
bare or pointy block returns raw in Raku anyway, and an anonymous `sub` is built
from an `Expr::AnonSubParams`, which records `is_rw` but has no field for
`is_raw` — so `sub () is raw { ... }` is indistinguishable there from a plain
one and keeps the pre-existing no-FETCH answer.

## Two bugs the fix uncovered

**`try` did not evaluate its value.** With the container now reaching the
caller, `(try dying-fetch-proxy()).defined` would FETCH *outside* the `try` and
escape it. Rakudo evaluates a genuine `try`'s value inside the protected region
— a throwing FETCH is caught and the `try` yields `Nil`, while an untroubled one
still hands the `Proxy` back (`my $p := try f(); $p.VAR.^name` is `Proxy`). The
`TryCatch` op now probes its body's value the same way, which also fixes the
same escape for an `is raw` routine, where it was reachable before this change.

**A captured `Proxy` never ran `STORE`.** An assignment inside a block or a
named sub reaches the variable by name (`SetGlobal`), never through a local
slot, and that path replaced the captured container with the plain value instead
of running `STORE` — the write silently vanished and the program carried on:

```raku
my $cell = 0;
my $p := Proxy.new(FETCH => method () { $cell }, STORE => method ($n) { $cell = $n });
my $b = { $p = 2 }; $b();      # raku: $cell is 2;  mutsu: still 0
sub s1() { $p = 3 }; s1();     # raku: $cell is 3;  mutsu: still 0
```

The store now checks for a `Proxy` before both write-throughs on that path (the
compunit-lexical store and the generic `ContainerRef` one), each of which would
otherwise rebind the name rather than write through it. This was live on `main`
independently of the return rule; it stayed hidden because a bound name almost
never held a real `Proxy` before.

## Pin

`t/proxy-rw-routine-return-bind.t` (25 tests, verified identical under rakudo):
read, write and read-after-write through the binding; `FETCH`/`STORE` counted
exactly once per access, including in interpolation and as a sub argument; a
`Proxy` returned from a method behaving as one returned from a sub; a plain
`sub` still decontainerizing its return; `return-rw` without either trait; the
`try` probe; and the three captured-`Proxy` store shapes.

`t/thread-shared-scalar-visibility.t`'s "STORE through a captured Proxy is
visible" loses its `todo`: a `start` block's write is one of the captured-name
stores the second fix repairs.

Closes #7748.
