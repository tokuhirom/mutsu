# Cro's HTTP client: a wrapper's invocant, and `if EXPR -> @a`

Loading `Cro::HTTP::Client` worked, but the moment a request was made every
client-side test file died with `No such method 'lock' for invocant of type
'Any'`, taking six Cro::HTTP suite files down with it (`http-auth-basic`,
`http-auth-basic-with-session`, `http-session-inmemory`,
`http-session-persistent`, and the two webtoken files). The message came from
OO::Monitors' method wrapper, but the monitor machinery was innocent: two
unrelated general bugs were behind it.

## A method wrapper bound its invocant to the first argument

`OO::Monitors` wraps every method of a `monitor` with

```raku
$meth.wrap: -> \SELF, | {
    my $lock = $!lock-attr.get_value(SELF);
    $lock.lock();
    ...
}
```

A method wrapper is invoked with the invocant *prepended* to the method's own
arguments, so the wrapper's first parameter is the invocant. mutsu built that
argument list correctly — but the pending *call-site argument source names*
(the caller's variable name behind each argument, which the call opcode records
so an `is rw` / `is raw` / sigilless parameter can alias and write back to it)
were recorded for the method's arguments only, and were never shifted to
account for the prepended invocant.

A sigilless parameter re-reads its value from the named source variable rather
than from the argument slot, so `\SELF` picked up source name `[0]` — which
named the method's *first argument*, not the invocant. Cro's
`$!connection-cache.pipeline-for($secure, $host, $port, $http)` therefore ran
the wrapper with `SELF` bound to `$secure`, a `Bool`, and looked for the
monitor lock attribute on it. The shape is trivially reproducible with no
module at all:

```raku
class C { method who($a, $b) { } }
C.^lookup('who').wrap: -> \SELF, |c { say SELF.^name; callsame };
my $x = 1;
C.new.who(1,  2);   # C   -- literal argument, no source name, so it worked
C.new.who($x, 2);   # Int -- bare variable argument, and SELF became $x
```

The fix shifts the pending arg sources by one whenever a wrapper is entered
with the invocant prepended. A companion problem surfaced immediately: the
outermost wrapper *consumes* the pending sources when its own signature binds,
so a `callsame` reaching the wrapped original found none and rejected any
`is rw` parameter with `X::Parameter::RW`. The wrap-dispatch frame now carries
the original call's sources and restores them for each callee in the chain
(shifted again for an inner wrapper, unshifted for the original routine), so
`E.^lookup('bump').wrap(...)` no longer breaks `method bump($n is rw)`.

Pin: `t/wrap-invocant-arg-source.t`.

## `if EXPR -> @a { }` tested the bound container, not the condition

With the invocant fixed, the next request died on `No such method 'dead' for
invocant of type 'Any'` inside the connection cache:

```raku
if $http ne '2' && %!cached-http1{$key} -> @available {
    while @available {
        my $pipeline = @available.shift;
        return $pipeline unless $pipeline.dead;
    }
}
```

`%!cached-http1` was empty, so the condition was an undefined `Any` and the
branch must not run. mutsu desugared the pointy binding to `my @available =
EXPR` and then tested `@available` — and `my @a = Any` is a one-element
`[Any]`, which is *true*. So the block ran, `while @available` was true once,
and `.shift` handed out the `Any` that `.dead` was then called on. A missing
hash element is enough to show it:

```raku
my %h;
if %h<nope> -> @a { say "entered" } else { say "else" }   # mutsu said "entered"
```

The condition is now evaluated once into a hidden scalar which is what gets
tested, and the container is bound (not assigned) *inside* the taken branch —
binding matters because a pointy parameter aliases its argument, so
`if %cache<full> -> @avail` sees all three elements rather than a
one-element array holding the array; and deferring it matters because binding
a non-`Positional` condition (`if 0 -> @a`) is a type error that must not fire
when the branch is not entered. All three `if` compilation paths (statement,
value, and `do`-expression) share the helper, so `elsif` bindings get it too.

Pin: `t/if-pointy-container-param.t`.

## Effect on the Cro::HTTP suite

The four files that produced no TAP output at all now load, plan, and run their
assertions (`http-auth-basic` and `http-auth-basic-with-session` reach
`plan 5`; the two session files reach their first assertions), and the client
completes a request without dying in the connection cache. The remaining
failures there are the real client/server round-trip, which is the next
blocker.
