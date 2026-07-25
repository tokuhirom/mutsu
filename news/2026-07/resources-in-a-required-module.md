# `%?RESOURCES` in a module loaded by `require` resolved against the wrong distribution

`HTTP::UserAgent` could not fetch an `https://` URL. It reported

```
Runtime error: An exception occurred while evaluating a CHECK
Exception details:
  Please install IO::Socket::SSL in order to fetch https sites
```

even though `IO::Socket::SSL` is bundled and working — adding a bare
`use IO::Socket::SSL;` to the script made the very same request return 200.

## What was actually wrong

`HTTP::UserAgent.get-connection` loads the TLS socket lazily:

```raku
try require ::("IO::Socket::SSL");
die "Please install IO::Socket::SSL …" if ::('IO::Socket::SSL') ~~ Failure;
```

The `try` swallowed the real error. Unwrapped, it was
`No such method 'slurp' for invocant of type 'Any'`, thrown from
`OpenSSL::NativeLib`:

```raku
BEGIN my %libraries = Rakudo::Internals::JSON.from-json: %?RESOURCES<libraries.json>.slurp(:close);
```

`%?RESOURCES` is lexically tied to the compilation unit that contains the token,
so this must resolve against the *OpenSSL* distribution. mutsu instead preferred
the distribution of the innermost routine on the call stack — a rule added so
that a module's method reading `%?RESOURCES` while *another* module is still
loading gets its own distribution (the MIME::Types case). But a module's own
mainline and `BEGIN` blocks run with **no frame of their own**, so the innermost
frame is then whoever triggered the load: `HTTP::UserAgent.get-connection`. Its
distribution has no `libraries.json`, so the lookup was `Any` and `.slurp` blew
up. Because the failure happened at `BEGIN` time it surfaced wrapped as
"An exception occurred while evaluating a CHECK", hiding the cause.

`use IO::Socket::SSL` worked because the whole chain then loads at the script's
compile time, with no routine frame in the way.

## The fix

A module load now records the `routine_stack` height at which it established
`current_distribution` (`current_distribution_frame_floor`). The
routine-stack rule only considers frames at or above that floor — those were
pushed by code the loading module itself called, which is exactly the
MIME::Types shape it was written for. Frames below belong to the caller that
triggered the load, and no longer shadow the module being loaded.

`$ua.get("https://example.com/").code` now returns 200 with no extra `use`.
Pinned by `t/resources-in-required-module.t`, whose fixture distributions live
under `t/lib/ResCaller`, `t/lib/ResDist` and `t/lib/ResInner`.
