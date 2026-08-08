# Constructing an object no longer overwrites the caller's `$_`

```raku
class S { has Bool $.b }
$_ = 3.7e0;
S.new;
say $_.^name;    # raku: Num    mutsu: Bool
```

The topic came back as the *type object of the last unset typed attribute*: a
class with `has Int $.i; has Rat $.r` left `$_` holding `Rat`, and an untyped
attribute or one with a default left it alone.

## Root cause

Seeding an unset typed attribute evaluates its declared type through
`eval_decl_trait_arg` → `vm_eval_block_value`, which compiles the expression as a
block **for its value** — so its last expression is a `SetTopic`. That helper
runs at declaration time, inside whatever frame happens to be constructing, so
the topic write escaped straight into the caller.

## Fix

`vm_eval_block_value` saves and restores `$_` around the nested run (both the
compiled fast path and the interpreter fallback). The value it produces is
unchanged.

Pinned by `t/decl-time-value-block-keeps-the-topic.t`.

## Effect

Cro's cookie jar hit it in a loop body:

```raku
for $resp.cookies {
    $state = CookieState.new(creation-time => DateTime.now, …);
    self!get-cookie-lifetime($_, $state);      # $_ is now Bool
}
```

`CookieState` has four `Bool` attributes, none of them passed, so `$_` stopped
being the `Cro::HTTP::Cookie` and the private call failed its signature — which
mutsu reports, misleadingly, as `No such private method 'get-cookie-lifetime'`.

`t/http-session-inmemory.rakutest` goes from 2 tests to **13 run, 6 passing**.
