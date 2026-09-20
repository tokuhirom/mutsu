# Selkie::UI: a method's own dispatcher, a role's own scope, and Iterable without `iterator`

`Selkie::UI` 0.0.4 was drawn from the ecosystem roulette at `red`: 0 of 6 baseline files at parity,
every module loading cleanly but every test file dying early. Rakudo passes all six. Three general
interpreter bugs accounted for five of them.

## A `method new` had no dispatcher of its own

Every Selkie widget constructor is written the same way:

```raku
method new(*%args --> Selkie::Widget::Spinner) {
    %args<focusable> //= False;
    callwith(|%args);
}
```

and the tests call them from inside `subtest`, which is a `multi sub`. mutsu answered

```
Cannot resolve caller subtest(Bool:D, Str:D); none of these signatures matches:
```

`callwith` was resolving against **`subtest`'s** candidate list. In Raku, `Mu.new(*%attrinit)` is
always the final candidate of a user `method new`'s MRO, but mutsu implements it natively (`bless`),
so it is not a `MethodDef` and never reached the deferral candidate list. With no candidates,
`push_method_dispatch_frame` pushed no frame at all, and `dispatch_next_candidate` then resolved to
the innermost frame that *did* exist — the enclosing multi sub's.

That the leak is always wrong is worth stating precisely, because the two directions differ in Raku
and both were checked against `raku`: a plain `sub` called from inside a multi **does** see the
enclosing dispatcher (`callsame` in it defers to the next `multi` candidate), but a **method** call
always establishes a dispatcher of its own, so `callsame` inside a method body can never reach out
to an enclosing routine's.

The fix follows the existing `mu_base_override` precedent in `push_method_dispatch_frame` — the same
one that already exists for `BUILDALL`/`POPULATE`/`clone`: a user `method new` now establishes an
(empty) MRO frame, and `native_mu_base_next_candidate` grew a `new` leg that reaches the nearest
builtin ancestor's native constructor, then `bless`. Pushing a frame on every `.new` would be a hot-
path cost for nothing, so it is gated on a new process-global `DISPATCHER_SEEN` latch
(`src/opcode.rs`), set when any compiled chunk anywhere in the program mentions
`callsame`/`nextsame`/`callwith`/`nextwith`. A program that never defers keeps the single-candidate
fast path for every constructor call.

Pinned by `t/oo/method/method-new-dispatcher-under-outer-multi.t`. This alone took `t/02`, `t/03`,
`t/05` and `t/08` from dying to passing.

## A composed role method could not see its own compunit

`Selkie::Widget` is a `unit role` that imports `Selkie::Alpha`'s `AlphaMode` enum. Inside the role,
`$!applied-fg-alpha = AlphaOpaque` died with

```
Type check failed in assignment to $!applied-fg-alpha; expected AlphaMode but got Str ("AlphaOpaque")
```

— the bareword had fallen through every resolution branch to its last resort, a plain string.

Role methods are registered with `lexical_package: self.current_package_sym()`, which is `GLOBAL`
while a `unit role` body runs. Composition then rewrites the method's owner to the *consuming class*.
Between the two, nothing on the running frame named the role, so `running_package_candidates`
probed only `["Consumer", "GLOBAL"]` and the role compunit's own bare terms — imported `constant`s,
enum keys, type names, all keyed by package in `module_scope_lexicals` — were unreachable from every
composed method body.

A role method is lexically inside the role, and that is the only anchor its body can use, so
`registration_role_method.rs` now records the role as the method's `lexical_package`.
`lookup_in_package_chain` walks up the `::` chain, so a role nested in a package still reaches that
package's entries. Pinned by `t/modules/compunit/role-method-sees-its-own-compunit-scope.t` with
three new fixtures under `t/lib/`.

## `does Iterable` with no `iterator` method

`Selkie::UI::ReactiveArray` is `does Positional does Iterable` and provides `list`, `AT-POS` and
`elems` — but no `iterator`. `for @a` over one yielded the object itself as a single element.

Rakudo composes `Iterable` on top of `Any`, whose `iterator` is `self.list.iterator`, so a class that
declares none inherits that — `does Iterable` plus `list` is enough to decompose. mutsu's iteration
helpers (`try_iterable_instance_items`, `try_user_iterator_items`) all gated on
`has_user_method(cn, "iterator")` and gave up otherwise. Both now fall back to driving `.list`, kept
on the Iterable-role gate in both places: measured against `raku`, a plain class with only a `list`
method is still one item to `for` and to `.map`. Pinned by
`t/oo/role/iterable-role-without-iterator-method.t`, which asserts that negative case too.

## Result

`Selkie::UI` 0.0.4 goes from `red` (0/6 baseline files) to `partial` (5/6). The remaining file,
`t/04-data-visualization.rakutest`, is blocked by the same class of scoping bug as the second fix
above, but in the attribute-*default* direction, where there is no routine frame to anchor: a role
attribute's default expression runs in the consuming class's package scope, so a bare type name the
role imported degrades to a `Str`. An anchor-only fix was prototyped, measured not to be sufficient,
and reverted rather than shipped half-done; the finding is [#8842](https://github.com/tokuhirom/mutsu/issues/8842).
