# A topic alias no longer fires in another frame

```raku
given Cro::HTTP::Client.new -> $client {
    note $client.^name;                      # Cro::HTTP::Client
    my $r1 = await $client.get("$url/hits");
    note $client.^name;                      # Expecting          <-- wrong
}
```

The response itself was correct — only the binding was destroyed. `Expecting` is
`Cro::HTTP::ResponseParser`'s `my enum Expecting <StatusLine Header Body>`,
declared inside its `supply` block, so a lexical from a completely different
frame landed in the caller's `$client`. The next `$client.get` then died with
`No such method 'get' for invocant of type 'Int'`, which is how Cro's session
tests failed their second assertion.

## Root cause

`given EXPR -> $y { … }` binds its parameter as `y := _`, and a `:=` bind
records `__mutsu_sigilless_alias::y = "_"` so that a later write to the source
propagates to the alias. That table is process-global — it is even mirrored into
the cross-thread shared store — but the reverse-alias propagation in
`exec_one_dispatch` applied it unconditionally: **any** `$_ = …`, in any frame or
thread, wrote into `$y`.

Cro's response parser is a supply body whose loop is literally
`loop { $_ = $expecting; when StatusLine { … } }`, so every state advance
overwrote the caller's `$client`.

The bare-name shared-variable collision family
(`session-shared-store-bare-name-collision`) was ruled out early: the variable
name does not matter, and `my $client = …` in a bare block is correct where
`given … -> $client` is not.

## Fix

An alias only means anything in the frame that made the binding, so propagate
only to a name that frame owns as a slot. Same-frame aliasing (`my $alias := $_;
$alias = 'x'` — the topic sees `'x'`) is unaffected, because that target *is* a
local of the assigning frame.

raku agrees on the semantics from the other direction too: in
`given $v -> $y { $_ = 5 }`, `$y` is still 1.

Pinned by `t/topic-alias-does-not-cross-frames.t` (callee topic write, a callee
driving a `when`-loop off its topic, a topic write on another thread, and the
same-frame alias that must keep working).

## Effect

`t/http-session-inmemory.rakutest` goes from 0 to 2 passing tests. The next
failure there is unrelated: `No such private method 'get-cookie-lifetime' for
invocant of type 'Cro::HTTP::Client::CookieJar'` — private-method dispatch on a
monitor.

## How it was found

`Env::insert`/`insert_sym` were temporarily instrumented behind an env var to
print a backtrace whenever a chosen key was written (the pattern the debugging
guidelines describe for "which caller wrote this"). That pointed straight at the
reverse-alias loop; a second print of `(assigned name, reverse target)` showed
`assigning _ -> reverse target client`, which named the mechanism outright. Note
that the String-keyed `Env::insert` does not route through `insert_sym`, so both
had to be hooked — hooking only one hid the first, decisive write.
