# Six more general fixes from the HTTP::UserAgent suite (19 → 23 files)

The second pass over the upstream HTTP::UserAgent test suite (27 files,
`raku-community-modules/HTTP-UserAgent` at `1d6a31a`) took it from **19 to 23 fully-passing
files, with no file left erroring out**. As in the first pass, every fix is a general
interpreter bug the module merely surfaced first.

## `IO::Socket::INET` kept the `:port` suffix on the host

`IO::Socket::INET.new(:host("localhost:8080"), :port(8080))` resolved
`localhost:8080:8080`. mutsu split a `host:port` string only when no `:port` was passed;
rakudo's constructor splits the host unconditionally and merely *defaults* the port from the
suffix (`%args<port> //= $port`), so an explicit `:port` wins but the host never keeps the
suffix. HTTP::UserAgent passes both — `$request.host` is `"localhost:8080"` while
`$request.port` is `8080`. Pin: `t/socket-inet-host-port-split.t`.

## An `is rw` method only exposed an attribute from its *first* statement

An `is rw` routine returns its last expression's container, so a body that does some work
before naming `$!attr` is still an assignable lvalue. mutsu only recognised the attribute when
it was the body's first statement, so HTTP::Request's lazily-defaulting

```raku
multi method scheme(--> Str:D) is rw {
    without $!scheme { … }
    $!scheme
}
```

threw `X::Assignment::RO: rw method 'scheme' does not expose an assignable attribute`. The
detection now reads the *last* statement (plus any explicit `return-rw $!attr` anywhere in the
body), which also makes `method computed is rw { $!v; 1 }` correctly non-assignable. Pin:
`t/rw-method-trailing-attribute-lvalue.t`.

## A Buf compared numerically as 0

`Buf`/`Blob` is Positional, so it numifies to its element count — `+$buf` and `.Numeric`
already agreed, but the infix numeric comparisons read an Instance as 0, so
`Buf.new(1,2,3,4,5) == 5` was False. TestServer gates its whole response on
`$in-buf.subbuf($header-end + 4) == $length`, so the local server never replied and the
binary-request tests hung. Pin: `t/buf-numeric-context.t`.

(Known remaining divergence: rakudo makes the *ordering* comparators throw for a Buf
— `$buf < 6` is `Cannot resolve caller Real(Buf:D:)` — where mutsu now compares element
counts. That is closer than the previous silent 0, and equality is what code actually uses.)

## `m-meta-ok` parsed as a `m` match with a `-` delimiter

A raku identifier may contain `-` and `'` when an alphabetic follows, and that beats the
quoting constructs: `m-meta-ok` is a call to the routine `m-meta-ok`, and `q-a-b`,
`tr-a-b-c`, `s-a-b`, `Q-a-b` likewise. mutsu accepted any non-word character as a delimiter,
so `m-meta-ok()` became `m-meta-` plus the stray adverb `ok` ("Unsupported use of /ok").
A `-` NOT followed by an alphabetic still *is* a delimiter, so `m-1-` keeps matching — the new
`delim_is_identifier_continuation` helper applies exactly raku's identifier rule at every
quote-op delimiter site (`m`/`rx`/`s`/`S`/`ss`, `tr`/`TR`, `q`/`qq`/`Q`). Pin:
`t/quote-op-hyphen-identifier.t`.

## A `CATCH` inside a BEGIN phaser did not catch a call

`BEGIN { f(); CATCH { … } }` let an exception thrown *inside* `f` escape as
`X::Comp::BeginTime` ("An exception occurred while evaluating a CHECK"); only a `die` executed
at the phaser's own statement level was caught. The phaser body compiles inline into the
enclosing mainline code, so the handler covered the mainline scope rather than the phaser. A
body that declares a handler now gets its own block scope — which is also the scope raku gives
it, since a `my` inside `BEGIN { … }` is block-scoped either way. Handler-less phasers keep
their inline, scope-less shape. Pin: `t/begin-phaser-catch-handler.t`.

## `require Test::Anything` silently succeeded

`use Test::X` deliberately treats a missing module as a no-op (roast's `Test::Util` pulls in
helpers that need not exist). `require` inherited that, so `require Test::META <&meta-ok>`
reported `X::Import::MissingSymbols` instead of the `X::CompUnit::UnsatisfiedDependency` its
callers catch to skip themselves. The swallow is now scoped to `use`: `require` propagates the
missing-module failure. Same pin as above.
