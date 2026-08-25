# Binding a sub-returned `Proxy`: the writability gap is gone, only the exception class differs

The ticket recorded that

```raku
sub double() {
    my $storage = 0;
    Proxy.new(
        FETCH => method ()     { $storage * 2    },
        STORE => method ($new) { $storage = $new },
    )
}
my $doubled := double();
$doubled = 4;
say $doubled;
```

printed `8` under `raku` while mutsu died with `Cannot assign to a readonly
variable (doubled) or a value` — i.e. binding a `Proxy` that came back from a
sub call lost its writability.

Re-measured on `main` @ `17139dd55` against `raku` v2026.06: **both now die.**
`raku` throws `X::AdHoc` / "Cannot assign to an immutable value"; mutsu throws
`X::Assignment::RO` / "Cannot assign to a readonly variable (doubled) or a
value". With the assignment wrapped in `try`, both then read the Proxy back as
`0`. The writability divergence the ticket was about no longer exists.

It is worth being precise about *why* it closed: this is Rakudo-side drift, not
a mutsu fix. `raku` used to accept the assignment and no longer does, so the two
implementations converged from opposite directions. Binding a `Proxy` directly
(no sub call in between) still works in both and still yields `8`, which is the
control that shows Proxy writability itself is intact:

```
$ mutsu -e 'my $s = 0; my $p := Proxy.new(FETCH => method () { $s * 2 }, STORE => method ($n) { $s = $n }); $p = 4; say $p;'
8
```

The one surviving difference — `X::Assignment::RO` where Rakudo uses `X::AdHoc`
— is not specific to Proxies at all. It is exactly the divergence already
tracked by `todo/tickets/readonly-variable-assign-uses-ro-instead-of-adhoc.md`,
which catalogues the same substitution for `my $x := 42; $x = 23`, for readonly
loop aliases, and for readonly sub parameters, along with the cases where Rakudo
genuinely does use `X::Assignment::RO`. Fixing that ticket fixes this case with
it, so this one is closed rather than kept as a duplicate.
