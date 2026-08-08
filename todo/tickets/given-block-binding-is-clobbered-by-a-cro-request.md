# A `given … -> $var { }` binding is clobbered by making a Cro request

```raku
# tmp/croclient3.raku — needs the vendored Cro tree (see the Cro campaign notes)
given Cro::HTTP::Client.new -> $client {
    note "A: ", $client.^name;                     # Cro::HTTP::Client
    my $r1 = await $client.get("$url/hits");
    note "B: ", $client.^name;                     # Expecting        <-- wrong
    my $b1 = await $r1.body-text;
    note "C: ", $client.^name, " body=", $b1.raku; # Expecting, body "Visit 1"
}
```

The response itself is correct — only the binding is destroyed. `Expecting` is
`Cro::HTTP::RequestParser`'s `my enum Expecting <RequestLine Header Body>`,
declared inside its `supply` block, which runs on the *server* side of the same
process. So a lexical from another frame (and another thread) lands in the
caller's `$client`.

This is what makes `t/http-session-inmemory.rakutest` fail its second test with
`No such method 'get' for invocant of type 'Int'`: the test wraps its client in
exactly this shape.

## What is established

* **The variable name does not matter.** Renaming `$client` to `$zzzclient`
  reproduces identically, so this is not the bare-name shared-variable collision
  family (`session-shared-store-bare-name-collision`); an index or slot is being
  applied to the wrong frame.
* **`given … -> $var` is what breaks.** Replacing it with `my $client = …`
  and a bare block makes the whole script correct:

  ```raku
  my $client = Cro::HTTP::Client.new;
  {
      note "A: ", $client.^name;   # Cro::HTTP::Client
      my $r1 = await $client.get("$url/hits");
      note "B: ", $client.^name;   # Cro::HTTP::Client  -- correct
  }
  ```
* **The first request is enough** — the clobber happens during
  `await $client.get(…)`, not on a later one.
* It is not the monitor topic/`self` leak
  (`news/2026-08/monitor-method-no-longer-leaks-topic-and-self.md`): measured on
  a build that includes that fix.

## Reduced repros that do NOT reproduce it

- a `given C.new -> $c` around `await start { … }` where the started sub
  declares `my enum Expecting` and assigns a `my $expecting`;
- the same where the enum and the assignment live in a `supply` block with a
  `whenever` fed from a `start`, with a nested `my sub` doing the assignment.

So the synthetic route needs something more of the real setup — most likely the
real socket/threading path. Per the project's usual method the next step is a
shadow bisect of the real `Cro/HTTP/RequestParser.rakumod` (copy it into a tree
that `-I` puts first — `tmp/croshadow.sh` — and delete statements until the
clobber stops), starting by removing the `my enum Expecting` declaration itself.

## Why it matters

It is the last known blocker for Cro's session tests, and a `given …` around a
client is idiomatic enough that other Cro test files will hit it too.
