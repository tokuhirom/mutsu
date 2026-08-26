# A leaked `skip_pseudo_method_native` flag made `$junction.raku` un-quote its first eigenstate

```
$ raku  -e 'my $r = any("5","6"); say $r.raku'
any("5", "6")
$ mutsu -e 'my $r = any("5","6"); say $r.raku'
any(5, "6")
```

Only `.raku` (not `.perl`, not `.gist`), only through a variable receiver, only
the first eigenstate. The ticket named four duplicate Junction-repr sites and
recommended `rust-gdb` before editing anything. That was the right call — none
of the four was to blame.

## Root cause

Breaking on the junction renderer in `methods_call_dispatch.rs` showed the
*correct* implementation running, looping over the eigenstates and calling
`.raku` on each. The first element's nested `.raku` returned `5`; the second
returned `"6"`. One breakpoint later the asymmetry was explicit:
`bypass_native_fastpath` was `true` for the first element and `false` for the
second, i.e. a one-shot piece of interpreter state was being consumed by the
first nested dispatch.

That state is `skip_pseudo_method_native`. It exists for exactly one purpose: a
*quoted* MOP pseudo-method call (`$obj."WHAT"()`) must dispatch a user-defined
method of that name instead of the reflection macro, and
`dispatch_method_by_name_1` consumes the flag once per dispatch. Its
`CallMethod` implementation (`vm_call_method_ops.rs`) sets it only when the call
is quoted *and* the name is one of the eight pseudo-methods. Its `CallMethodMut`
twin (`vm_call_method_mut_ops.rs`) set it for **any** `skip_native`, of which
"the receiver is a Junction and the method is `gist`/`raku`/`perl`" is one.

So `$r.raku` on a variable set the flag to `"raku"`; the renderer's first
`"5".raku` matched the name, bypassed the native repr, and fell through to
`dispatch_instance_and_fallback`'s stringifying `_ => target.to_string_value()`
catch-all — printing `5`. The flag was then cleared, so every later eigenstate
rendered correctly. `.perl` was unaffected because the flag held `"perl"` while
the renderer maps `perl` onto a nested `raku`; `.gist` was affected but
invisibly, since a gisted `"5"` is `5` anyway.

## Fix

`CallMethodMut` now gates the flag exactly as `CallMethod` does — quoted, and
one of the eight pseudo-method names. Nothing else about `skip_native` changes;
it keeps steering that call's own dispatch, it just no longer leaks a
method-name veto into the next nested dispatch of the same name.

Pinned by `t/lazy-gather-and-junction.t`, with order-free assertions (count the
quote characters, look for each quoted member) rather than a fixed eigenstate
order.
