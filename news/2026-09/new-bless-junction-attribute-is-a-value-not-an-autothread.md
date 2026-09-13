# `new`/`bless` store a Junction attribute argument instead of autothreading it

Found while root-causing `Chronic` for the ecosystem timeout cluster (#7995)
— this was that distribution's whole root cause, and it is general.

```raku
class E { has $.a; has $.b }
my $e = E.new(a => any(1,2), b => any(3,4));
say $e.a.raku, " / ", $e.b.raku;
```

`raku` answers `any(1, 2) / any(3, 4)`. mutsu answered `any(any(1, 1),
any(2, 2)) / any(any(3, 4), any(3, 4))` — a *nested* junction. A typed
attribute (`has Junction $.a`) made the bug fatal instead of silent: mutsu
died with a type check failure, since each threaded call passed one
eigenstate (an `Int`), never the junction itself.

`new`/`bless` are not a dispatch position at all in rakudo when using the
default constructor: named arguments are collected straight into
`*%attrinit`, so a `Junction` argument is a plain value to store. mutsu's
method-argument autothread check (`maybe_autothread_method_args`,
`src/vm/vm_call_autothread.rs`) never special-cased this: the receiver is
the type object (`ValueView::Package`), which its existing
"resolve the user method and respect its own parameter rules" branch only
ever checked for `ValueView::Instance`, so a `Package` receiver fell
through to the fallback "we don't know this method, thread everything"
behaviour — appropriate for an unresolvable ordinary method, wrong for the
default constructor specifically.

The fix reads the class name off either `Package` or `Instance` for
`new`/`bless` and resolves a user-defined override the same way the
existing branch does for other methods — a class that defines its own
`new`/`bless` is an ordinary method dispatch and keeps threading per its own
parameter types (verified: a typed positional parameter still autothreads
normally). When no override resolves, the call is the default constructor's
`*%attrinit` collection, so the junction is never threaded at all.

This also fixes the performance angle raised alongside the correctness bug:
threading was `O(eigenstates^junctions)`, since each junction argument
multiplied the number of constructor calls. `Chronic::Description` builds
five `Junction`-typed attributes from cron fields (up to 60 eigenstates
each) and ends its `new` in `self.bless(|%new-args)`, so the five-argument
call used to run the constructor across a five-dimensional product —
`t/035-cronspec.t` and `t/040-at.t` were recorded as "never finishes" in
#7995's 300s re-measurement. A two-junction, 60-eigenstate `.bless` now
returns in under a millisecond.

`t/concurrency/thread-lock/new-bless-junction-no-autothread.t` pins the
typed-attribute, untyped-attribute, `.bless`, user-defined-override, and
performance shapes.

[#8355](https://github.com/tokuhirom/mutsu/issues/8355)
