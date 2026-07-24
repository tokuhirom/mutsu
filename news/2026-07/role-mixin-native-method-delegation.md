# A role mixed into a native-backed instance keeps its native methods

Mixing a role into a native-backed instance (`$path does R`, `$sock does
Connection`, `"..." .IO but R`) made every *native* method of the base class
disappear:

```raku
role R { method g { 1 } }
my $p = "/foo/bar.txt".IO;
$p does R;
say $p.g;         # 1        — the role method was fine
say $p.basename;  # No such method 'basename' for invocant of type 'IO::Path'
```

## Root cause

`call_method_with_values` already ends with a Mixin fallback that delegates an
unhandled method to the inner value, and that fallback is what made mixins work
for user-defined classes. But three by-name native dispatchers
(`dispatch_method_by_name_1/2/3` — string, IO, coercion, collection, network)
run *before* it. Those dispatchers select on the method name and then key on the
target's runtime shape. Given the `Mixin` wrapper rather than the inner
`Instance`, they matched the name, failed to find a native receiver, and returned
a hard `X::Method::NotFound` — so control never reached the delegation at the tail
of the function.

Role methods were unaffected because `dispatch_mixin_method_call` runs earlier
still, which is why only the *native* half of a mixed value went missing.

## Fix

Delegate to the inner value just before the by-name dispatchers, but only when
the inner instance's class declares the method as a native one and does *not*
declare a user method of that name:

- `is_native_method(cls, method)` selects exactly the methods the by-name
  dispatchers would otherwise steal.
- `class_has_user_method(cls, method)` (user-declared `.methods` only — note that
  `class_has_method` also reports `native_methods`, so it is the wrong predicate
  here) leaves user-declared and inherited methods to the existing Mixin fallback,
  which runs them with `self` bound to the wrapper so nested `self.foo` still
  re-dispatches through the composed roles.

A role method that shadows a native name still wins, because
`dispatch_mixin_method_call` has already returned by the time this runs.

## Why it matters

This is the first blocker hit when driving the `HTTP::UserAgent` battery against
a real server. The module mixes a `Connection` role onto the socket
(`$conn does Connection`) and then calls the native `self.print(...)` /
`$conn.recv(:bin)` from the role's own `send-request`. Before the fix the request
could not even be written; with it, mutsu sends a real HTTP request and reads the
response back off the socket.

Pin: `t/mixin-native-method-delegation.t`.
